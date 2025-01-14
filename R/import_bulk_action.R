# Generic function for bulk actions, to reduce code repetition between validate/submit/edit functions

import_bulk_action <- function(project, action, method = NULL, token = mermaid_token()) {
  # Show messages
  silent <- FALSE

  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  check_single_project(project_id)

  # Check method when action is "edit"
  if (action == "edit") {
    if (!all(method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching"))) {
      stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"', call. = FALSE)
    }
  }

  # Get relevant records ----
  # Operating on...
  # Validate: all collecting records
  # Submit: all VALID collecting records
  # Edit: all SUBMITTED records, for the given method

  if (action %in% c("validate", "submit")) {
    relevant_records <- get_collecting_records(project_id, token)
  } else if (action == "submit") {
    relevant_records <- relevant_records %>%
      dplyr::filter(validations_status == "ok")
  } else if (action == "edit") {
    method_to_methods_endpoint(method)
    relevant_records <- get_project_endpoint(project_id, method_endpoint)
  }

  # Messaging if there are no relevant records to operate on ----
  if (nrow(relevant_records) == 0) {
    if (!silent) {
      no_records_message <- switch(
        "validate" = "No records in Collecting to validate.",
        "submit" = "No valid records in Collecting to submit.",
        "edit" = "No submitted records to edit.",
      )
    }

    usethis::ui_field(no_records_message) %>%
      print()

    return(invisible(NULL))
  }

  # Show a message that records are being validated/submitted/edited ----

  n_relevant_records <- nrow(relevant_records)
  n_relevant_records_plural <- plural(n_relevant_records)

  if (!silent) {
    n_message <- glue::glue("{n_validating} record{n_validating_plural} being {action_verb}...",
      action_verb = switch(
        "validate" = "validated",
        "submit" = "submitted",
        "edit" = "edited and moved back to Collecting",
      )
    )
    usethis::ui_field(n_message) %>%
      print()
  }

  # Validate/submit/edit records -----

  if (action %in% c("validate")) {
    # For validation, do in batches of 3
    batch_size <- 3
  } else if (action %in% c("submit", "edit")) {
    # For submit and edit, do one by one
    batch_size <- 1
  }

  relevant_records_split <- relevant_records %>%
    dplyr::mutate(...validate_group = ceiling(dplyr::row_number() / batch_size)) %>%
    split(.$...validate_group)

  # Set up progress bar
  if (!silent) {
    progress_bar <- list(format = "{cli::pb_bar} | {cli::pb_percent}") # Show progress bar, but not with ETA -> only % through
  } else {
    progress_bar <- FALSE
  }

  action_res <- purrr::map(
    relevant_records_split,
    .progress = progress_bar,
    \(x) {
      if (action == "validate") {
        validate_collect_records(x, project_id, token = token)
      } else if (action == "submit") {
        submit_collect_records(x, project_id, token = token)
      } else if (action == "edit") {
        edit_collect_records(x, project_id, method, token = token)
      }
    }
  ) %>%
    purrr::list_rbind()

  # Summarise results
  if (action == "submit") {
    # Just differentiate between ok/not_ok for submit
    action_res <- action_res %>%
      dplyr::mutate(status = ifelse(!identical(status, "ok"), "not_ok", "ok"))
  }

  statuses <- switch(
    "validate" = c("error", "warning", "ok"),
    "submit" = c("ok", "not_ok"),
    "edit" = c("ok", "not_ok")
  )

  action_res %>%
    summarise_all_statuses(statuses, action)
}
