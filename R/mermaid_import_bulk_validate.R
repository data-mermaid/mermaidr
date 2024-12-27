mermaid_import_bulk_validate <- function(project, token = mermaid_token()) {
  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  check_single_project(project_id)

  # Get all records in "collecting"
  # Validate ALL, even those that already have warning/error, so that summary is accurate to everything in collecting
  collect_records <- get_collecting_records(project, token)

  # Handle the case where there are no records to validate
  if (nrow(collect_records) == 0) {
    browser()
  }

  # Go through each and validate, one by one
  # The response returned contains the validation status of each
  validate_ids <- collect_records[["id"]]

  # Show a message that records are being validated

  n_validating <- length(validate_ids)
  n_validating_plural <- plural_space(n_validating)

  validating_msg <- glue::glue("{n_validating} record{n_validating_plural}being validated...")
  usethis::ui_field(validating_msg) %>%
    print()

  # Validate records
  validate_url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/validate/"))

  validation_res <- purrr::map(
    validate_ids, \(x) {
      validate_body <- list(ids = list(x))

      # Post validation
      response <- httr::POST(validate_url, encode = "json", body = validate_body, ua, token)

      # browser()

      if (httr::http_error(response)) {
        # If an actual error in sending the request, not the validation itself
        browser()
      } else {
        # Get the status
        dplyr::tibble(
          status = httr::content(response)[[1]][["status"]]
        )
      }
    }
  )

  names(validation_res) <- validate_ids

  validation_res <- validation_res %>%
    dplyr::bind_rows(.id = "id")

  validation_statuses <- c("error", "warning", "ok")

  validation_summary <- validation_res %>%
    dplyr::mutate(status = forcats::fct_expand(status, validation_statuses)) %>%
    dplyr::count(status) %>%
    tidyr::complete(status, fill = list(n = 0))

  # Summarise results
  purrr::walk(
    validation_statuses,
    \(status) validation_summary %>%
      summarise_status(status)
  )
}

validate_collect_record <- function(id) {

}

get_collecting_records <- function(project, token = mermaid_token()) {
  res <- mermaid_get_project_endpoint(project, "collectrecords")

  # Expand validations, just return the status and ID
  res %>%
    tidyr::unpack("validations", names_sep = "_") %>%
    dplyr::select(dplyr::all_of(c("id", "validations_status")))
}
summarise_status <- function(df, status) {
  n_status <- df %>%
    dplyr::filter(status == !!status) %>%
    dplyr::pull(n)

  plural <- plural_space(n_status)

  message <- dplyr::case_when(
    status %in% c("warning", "error") ~ glue::glue("{n_status} record{plural}produced {status}s in validation"),
    status == "ok" ~ glue::glue("{n_status} record{plural}successfully validated without warnings or errors")
  )

  switch(status,
    "error" = usethis::ui_oops(message),
    "warning" = usethis::ui_todo(message),
    "ok" = usethis::ui_done(message)
  )
}

plural_space <- function(x) {
  ifelse(x == 1, " ", "s ")
}
