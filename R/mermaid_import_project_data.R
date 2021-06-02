#' Import data into MERMAID Collect
#'
#' Import data into MERMAID Collect. By default this function just validates records to see if they can successfully be imported into Collect - if not, a warning is returned with validation errors. If the validation is successful, then running the function again with the setting \code{dryrun = FALSE} will actually import the records into Collect.
#'
#' @param data Data to import. Either a data frame or a file path.
#' @param project_id ID of project to import data into.
#' @param method Method to import data for. One of "fishbelt", "benthiclit", "benthicpit", "bleaching", "habitatcomplexity"
#' @param dryrun Whether the import is a dry run. If \code{TRUE}, records are validated, but nothing is saved in Collect. If \code{FALSE}, records are validated and validated records ARE saved to Collect. Defaults to \code{TRUE}.
#' @param token API token.
#'
#' @export
mermaid_import_project_data <- function(data, project_id, method = c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching"), dryrun = TRUE, token = mermaid_token()) {

  # Check if data is a data frame
  data_is_df <- inherits(data, "data.frame")

  if (data_is_df) {
    data_file_location <- tempfile(fileext = ".csv") # If it's a data frame, save to tempfile
    readr::write_csv(data, data_file_location)
  } else {
    csv_file <- FALSE
    data_is_chr <- is.character(data)

    if (data_is_chr) { # Check if the file exists and if it's a CSV
      csv_file <- fs::file_exists(data) & (fs::path_ext(data) == "csv")
    }

    if (!data_is_chr | !csv_file) { # Error if it's not a character vector (so no chance of being a file) or doesn't exist / isn't a CSV
      stop("`data` must be a data frame or path to a CSV file.", call. = FALSE)
    }

    data_file_location <- data
  }

  # Check project ID

  # Check method
  if (!method %in% c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching")) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "bleaching", "habitatcomplexity"', call. = FALSE)
  }

  # Construct ingestion URL
  ingest_url <- glue::glue("{base_url}/v1/projects/{project_id}/collectrecords/ingest/")

  method <- ifelse(method == "bleaching", "bleachingqc", method)

  # Create body, with settings from dryrun
  body <- list(file = httr::upload_file(data_file_location), protocol = method)

  if (dryrun) {
    body <- append(body, list(dryrun = "true"))
  }

  # Post data
  response <- httr::POST(ingest_url, encode = "multipart", body = body, ua, token)

  # Delete tempfile
  if (data_is_df) {
    fs::file_delete(data_file_location)
  }

  # Parse error / say successful

  if (httr::status_code(response) == 401) {
    check_errors(response)
  }

  if (httr::http_error(response)) {
    error <- httr::content(response, "text", encoding = "UTF-8")

    error_invalid_project <- stringr::str_detect(error, "is not a valid uuid")

    if (error_invalid_project) {
      stop("Failed to import data. '", project_id, "' is not a valid project_id.", call. = FALSE)
    }

    error_not_in_project <- stringr::str_detect(error, "ou are not part of this project")

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
        tidyr::unnest(cols = names(.))

      warning("Failed to import data. Problems:", call. = FALSE, immediate. = TRUE)

      return(error)
    } else {
      stop("Failed to import data: ", error, call. = FALSE)
    }
  }

  # Success message

  # If it was a dry run, tell them to run with dryrun = FALSE
  if (dryrun) {
    message("Records successfully validated! To import, please run the function again with `dryrun = FALSE`.")
  } else {
    # If it wasn't, tell them to go to Collect!
    collect_url <- glue::glue("{collect_url}/#/projects/{project_id}/collect",
                              collect_url = stringr::str_replace(base_url, "api", "collect")
    )
    message("Records successfully imported! Please review in Collect: ", collect_url)
  }
}
