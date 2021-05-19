mermaid_import_project_data <- function(file, project_id, method, dryrun = TRUE, token = mermaid_token()) {

  # Check file
  # If it's a data frame, save to tempfile

  # Otherwise upload file

  # If it's something else, error

  # Check project ID

  # Check method

  # Construct ingestion URL
  ingest_url <- glue::glue("{base_url}/v1/projects/{project_id}/collectrecords/ingest/")

  method <- ifelse(method == "bleaching", "bleachingqc", method)

  # Post data
  response <- httr::POST(ingest_url, encode = "multipart", body = list(file = httr::upload_file(file), protocol = method, dryrun = dryrun), ua, token)

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
}
