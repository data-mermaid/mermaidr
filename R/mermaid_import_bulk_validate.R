mermaid_import_bulk_validate <- function(project, token = mermaid_token()) {

  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  # Get all records in "collecting"
  collect_records <- get_collecting_records(project, token)

  browser()

  validate_url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/validate/"))

  validate_body <- list(ids = list("01c19cdd-a516-4325-8a68-2052d6fbe47d"))

  # Post validation
  response <- httr::POST(validate_url, encode = "json", body = validate_body, ua, token)

  # Go through each and validate, one by one
  # NOTE -> should we validate just those that are stale/NA?
  # "Doesn't hurt to revalidate" -> but shouldn't need to

  # Summarise results
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
