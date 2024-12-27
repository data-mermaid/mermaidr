mermaid_import_bulk_validate <- function(project, token = mermaid_token()) {
  # Show messages
  silent <- FALSE

  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  check_single_project(project_id)

  # Get all records in "collecting"
  # Validate ALL, even those that already have warning/error, so that summary is accurate to everything in collecting
  collect_records <- get_collecting_records(project, token)

  # Handle the case where there are no records to validate
  if (nrow(collect_records) == 0) {
    if (!silent) {
      usethis::ui_field("No records in Collecting to validate.") %>%
        print()
    }

    return(invisible(NULL))
  }

  # Go through each and validate, one by one
  # The response returned contains the validation status of each
  # Show a message that records are being validated

  n_validating <- nrow(collect_records)
  n_validating_plural <- plural_space(n_validating)

  if (!silent) {
    validating_msg <- glue::glue("{n_validating} record{n_validating_plural}being validated...")
    usethis::ui_field(validating_msg) %>%
      print()
  }

  # Validate records
  validate_url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/validate/"))

  # Do in batches of three
  batch_size <- 3
  collect_records_split <- collect_records %>%
    head(2) %>%
    dplyr::mutate(...validate_group = ceiling(dplyr::row_number() / batch_size)) %>%
    split(.$...validate_group)

  if (!silent) {
    progress_bar <- list(format = "{pb_bar} | {pb_percent}") # Show progress bar, but not with ETA -> only % through
  } else {
    progress_bar <- FALSE
  }

  validation_res <- purrr::map(
    collect_records_split,
    .progress = progress_bar,
    \(x) {
      if (nrow(x) == 1) {
        ids <- list(x[["id"]])
      } else {
        ids <- x[["id"]]
      }
      validate_body <- list(ids = ids)

      # Post validation
      response <- httr::POST(validate_url, encode = "json", body = validate_body, ua, token)

      if (httr::http_error(response)) {
        # If an actual error in sending the request, not the validation itself
        browser()
      } else {
        # Get the status
        httr::content(response) %>%
          purrr::map_dfr(
            .id = "id",
            \(x) dplyr::tibble(status = x[["status"]])
          )
      }
    }
  ) %>%
    purrr::list_rbind()


  validation_statuses <- c("error", "warning", "ok")

  validation_summary <- validation_res %>%
    dplyr::mutate(status = forcats::fct_expand(status, validation_statuses)) %>%
    dplyr::count(status) %>%
    tidyr::complete(status, fill = list(n = 0))

  # Summarise results
  if (!silent) {
    purrr::walk(
      validation_statuses,
      \(status) validation_summary %>%
        summarise_status(status)
    )
  }
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
