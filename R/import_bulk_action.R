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
  } else if (action == "edit") {
    method_to_methods_endpoint(method)
    relevant_records <- get_project_endpoint(project_id, method_endpoint)
  }

  if (action == "submit") {
    relevant_records <- relevant_records %>%
      dplyr::filter(validations_status == "ok")
  }

  # Messaging if there are no relevant records to operate on ----
  if (nrow(relevant_records) == 0) {
    if (!silent) {
      # TODO - maybe for "submit", ensure "validate" has been run first?

      no_records_message <- switch(
        action,
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
    n_message <- glue::glue("{n_relevant_records} record{n_relevant_records_plural} being {action_verb}...",
      action_verb = switch(
        action,
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
      if (action %in% c("validate", "submit")) {
        validate_or_submit_collect_records(x, project_id, action, token = token)
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
      dplyr::mutate(status = ifelse(status == "ok", "not_ok", "ok"))
  }

  statuses <- switch(
    action,
    "validate" = c("error", "warning", "ok"),
    "submit" = c("ok", "not_ok"),
    "edit" = c("ok", "not_ok")
  )

  action_res %>%
    summarise_all_statuses(statuses, action)
}

validate_or_submit_collect_records <- function(x, project_id, action, token = mermaid_token()) {
  url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/{action}/"))

  if (nrow(x) == 1) {
    ids <- list(x[["id"]])
  } else {
    ids <- x[["id"]]
  }
  body <- list(ids = ids)

  # Post submission
  response <- suppress_http_warning(
    httr::POST(url, encode = "json", body = body, ua, token)
  )

  if (httr::http_error(response)) {
    # If an actual error in sending the request, not the submission itself
    check_errors(response)
  } else {
    # Get the status
    httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      purrr::map_dfr(
        .id = "id",
        \(x) {
          dplyr::tibble(status = x[["status"]])
        }
      )
  }
}

summarise_all_statuses <- function(df, statuses, type = c("validate", "submit", "action")) {
  status_summary <- df %>%
    dplyr::count(status) %>%
    dplyr::mutate(
      status = forcats::fct_expand(status, statuses),
      status = forcats::fct_relevel(status, statuses)
    ) %>%
    tidyr::complete(status, fill = list(n = 0)) %>%
    split(.$status)

  if (type == "validate") {
    status_summary %>%
      purrr::walk(summarise_validations_status)
  } else if (type == "submit") {
    status_summary %>%
      purrr::walk(summarise_submit_status)
  } else {
    browser()
  }
}

plural <- function(x) {
  ifelse(x == 1, "", "s")
}

plural_were <- function(x) {
  ifelse(x == 1, "was", "were")
}

