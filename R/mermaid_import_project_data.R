#' Import data into MERMAID Collect
#'
#' Import data into MERMAID Collect. By default this function just checks records to see if they can successfully be imported into Collect - if not, a warning is returned with import errors. If there are no import errors, then running the function again with the setting \code{dryrun = FALSE} will actually import the records into Collect.
#'
#' @param data Data to import. Either a data frame or a file path.
#' @param method Method to import data for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"
#' @param dryrun Whether the import is a dry run. If \code{TRUE}, records are checked for errors, but nothing is saved in Collect. If \code{FALSE}, records are checked for errors and passing records ARE saved to Collect. Defaults to \code{TRUE}.
#' @param clearexisting Whether to remove ALL existing records for that method. Should only be used with extreme caution, if e.g. you made a mistake in import and want to overwrite ALL existing fishbelt / benthic PIT / etc records.
#' @inheritParams get_project_endpoint
#'
#' @export
mermaid_import_project_data <- function(data, project, method = c("fishbelt", "benthicpit", "benthiclit", "benthicpqt", "habitatcomplexity", "bleaching"), dryrun = TRUE, clearexisting = FALSE, token = mermaid_token()) {
  check_internet()

  project <- as_id(project)
  check_project(project)

  # Check only one project
  check_single_project(project)

  # Check if data is a data frame
  data_is_df <- inherits(data, "data.frame")

  data_file_location <- tempfile(fileext = ".csv")

  if (data_is_df) {
    # Save to tempfile
    # na = "" saves any NAs as empty strings, so they appear as e.g. "a",,"b" instead of "a","NA","b"
    utils::write.csv(data, data_file_location, row.names = FALSE, na = "")
  } else {
    csv_file <- FALSE
    data_is_chr <- is.character(data)

    if (data_is_chr) { # Check if the file exists and if it's a CSV
      csv_file <- file.exists(data) & (stringr::str_ends(data, "csv"))
    }

    if (!data_is_chr | !csv_file) { # Error if it's not a character vector (so no chance of being a file) or doesn't exist / isn't a CSV
      stop("`data` must be a data frame or path to a CSV file.", call. = FALSE)
    }

    # Read in the data, convert any NAs to "", then write to tempfile
    data <- utils::read.csv(data, check.names = FALSE)

    # Save to tempfile
    # na = "" saves any NAs as empty strings, so they appear as e.g. "a",,"b" instead of "a","NA","b"
    utils::write.csv(data, data_file_location, row.names = FALSE, na = "")
  }

  # Check project ID

  # Check method
  if (!method %in% c("fishbelt", "benthicpit", "benthiclit", "benthicpqt", "habitatcomplexity", "bleaching")) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"', call. = FALSE)
  }

  # Construct ingestion URL
  ingest_url <- glue::glue("{base_url}/v1/projects/{project}/collectrecords/ingest/")

  method <- ifelse(method == "bleaching", "bleachingqc", method)

  # Create body, with settings from dryrun
  body <- list(file = httr::upload_file(data_file_location), protocol = method)

  if (dryrun) {
    body <- append(body, list(dryrun = "true"))
  }

  if (dryrun & clearexisting) {
    stop("Import cannot be run with dryrun = TRUE and clearexisting = TRUE. dryrun = TRUE only checks records without submitting them, and clearexisting = TRUE will overwrite ALL existing ", method, " records. Please double check which option you would like to set.", call. = FALSE)
  }

  if (clearexisting) {
    clearexisting_confirm <- usethis::ui_yeah("Setting `clearexisting = TRUE` will overwrite ALL existing {method} records in Collecting.\nPlease only use this option if you would like to remove ALL {method} records in Collecting and replace them with the ones being imported. Would you like to continue?", yes = "Yes", no = "No", shuffle = FALSE)

    if (clearexisting_confirm) {
      body <- append(body, list(clearexisting = "true"))
    } else {
      return(message("Import stopped - please run again with `clearexisting = FALSE`."))
    }
  }

  # Post data
  response <- suppress_http_warning(
    httr::POST(ingest_url, encode = "multipart", body = body, ua, token)
  )

  # Delete tempfile
  if (data_is_df) {
    file.remove(data_file_location)
  }

  # Parse error / say successful
  error_code <- httr::status_code(response)

  if (error_code == 401) {
    check_errors(response)
  } else if (error_code == 504) {
    stop(
      "Request timed out due to the size of the data. Please split up your data (e.g. by site or date) and import each split section separately.",
      call. = FALSE
    )
  }

  if (httr::http_error(response) & error_code != 504) {
    error <- httr::content(response, "text", encoding = "UTF-8")

    error_invalid_project <- stringr::str_detect(error, "Not Found") | stringr::str_detect(error, "is not a valid uuid")

    if (error_invalid_project) {
      stop("Failed to import data. '", project, "' is not a valid project ID", call. = FALSE)
    }

    error_not_in_project <- stringr::str_detect(error, "ou are not part of this project") | stringr::str_detect(error, "ou do not have permission to perform this action")

    if (error_not_in_project) {
      error <- jsonlite::fromJSON(error)[["detail"]]
      stop("Failed to import data. ", error, call. = FALSE)
    }

    error_missing_fields <- stringr::str_detect(error, "Missing required fields")

    if (!error_missing_fields) { # Convert the JSON error to a data frame
      error <- jsonlite::fromJSON(error, simplifyDataFrame = FALSE) %>%
        purrr::transpose() %>%
        dplyr::as_tibble() %>%
        tidyr::unnest(cols = names(.)) %>%
        # Need to unnest all the columns twice
        tidyr::unnest(cols = names(.)) %>%
        # Rename row number, move to the front
        dplyr::rename(row_number = "$row_number") %>%
        dplyr::select(tidyselect::all_of("row_number"), dplyr::everything()) %>%
        # Subtract 1, since API version counts the header as row 1, which is unusual when working within R!
        dplyr::mutate(row_number = .data$row_number - 1)

      warning("Failed to import data. Problems:", call. = FALSE, immediate. = TRUE)

      return(error)
    } else {
      stop("Failed to import data: ", error, call. = FALSE)
    }
  }

  # Success message

  # If it was a dry run, tell them to run with dryrun = FALSE
  if (dryrun) {
    message("Records successfully checked! To import, please run the function again with `dryrun = FALSE`.")
  } else {
    message("Records successfully imported! Please review in Collect.")
  }
}
