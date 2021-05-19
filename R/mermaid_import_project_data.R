#' Import data into MERMAID Collect
#'
#' Import data into MERMAID Collect. If successful, the function returns a message saying that it was successful, and returns the data back to you. If unsuccessful, the function returns a data frame of validation errors.
#'
#' @param data Data to import. Either a data frame or a file path.
#' @param project_id ID of project to import data into.
#' @param method Method to import data for. One of "fishbelt", "benthiclit", "benthicpit", "bleaching", "habitatcomplexity"
#' @param dryrun Whether the upload is a dry run, defaults to \code{TRUE}. If \code{TRUE}, records are validated and valid Collect records are created but NOT saved to the database. If \code{FALSE}, records are validated and valid Collect reocrds are saved to the database.
#' @param token API token.
#'
#' @export
#'
#' @examples
mermaid_import_project_data <- function(data, project_id, method = c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching"), dryrun = TRUE, token = mermaid_token()) {

  # Check if data is a data frame
  data_is_df <- inherits(data, "data.frame")

  if (data_is_df) {
    df_temp <- tempfile(fileext = ".csv") # If it's a data frame, save to tempfile
    readr::write_csv(data, df_temp)
  } else {
    csv_file <- FALSE
    data_is_chr <- is.character(data)

    if (data_is_chr) { # Check if the file exists and if it's a CSV
      csv_file <- fs::file_exists(data) & (fs::path_ext(data) == "csv")
    }

    if (!data_is_chr | !csv_file) { # Error if it's not a character vector (so no chance of being a file) or doesn't exist / isn't a CSV
      stop("`data` must be a data frame or path to a CSV file.", call. = FALSE)
    }
  }

  # Check project ID

  # Check method
  if (!method %in% c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching")) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "bleaching", "habitatcomplexity"', call. = FALSE)
  }

  # Construct ingestion URL
  ingest_url <- glue::glue("{base_url}/v1/projects/{project_id}/collectrecords/ingest/")

  method <- ifelse(method == "bleaching", "bleachingqc", method)

  # Post data
  response <- httr::POST(ingest_url, encode = "multipart", body = list(file = httr::upload_file(data), protocol = method, dryrun = dryrun), ua, token)

  # Parse error / say successful

  if (httr::http_error(response)) {

    error <- httr::content(response, "text", encoding = "UTF-8")

    if(jsonlite::validate(error)) { # If the error is JSON, convert it to a data frame
      error <- jsonlite::fromJSON(error, simplifyDataFrame = FALSE) %>%
        purrr::transpose() %>%
        dplyr::as_tibble() %>%
        tidyr::unnest(cols = names(.)) %>% # Need to unnest all the columns twice
        tidyr::unnest(cols = names(.))

      usethis::ui_todo("Failed to import data. Error:")

      error
    } else {
      usethis::ui_todo("Failed to import data. Error: {error}")
    }
  }

  # Success message

  # Delete tempfile
  if (data_is_df) {
    fs::file_delete(df_temp)
  }
}
