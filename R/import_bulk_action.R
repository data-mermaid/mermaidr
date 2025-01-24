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
    stop_msg <- '`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"'
    if (is.null(method)) {
      stop(stop_msg, call. = FALSE)
    } else if (length(method) > 1) {
      stop(stop_msg, call. = FALSE)
    } else if (!method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching")) {
      stop(stop_msg, call. = FALSE)
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
    edit_methods_endpoint <- method_to_methods_endpoint(method)
    relevant_records <- get_project_endpoint(
      project_id,
      edit_methods_endpoint
    )
  }

  if (action == "submit") {
    relevant_records <- relevant_records %>%
      dplyr::filter(validations_status == "ok")
  }

  # Messaging if there are no relevant records to operate on ----
  if (nrow(relevant_records) == 0) {
    if (!silent) {
      no_records_message <- switch(action,
        "validate" = "No records in Collecting to validate.",
        "submit" = "No valid records in Collecting to submit. Have you run `mermaid_import_bulk_validate()`?",
        "edit" = "No submitted records to edit.",
      )
    }

    usethis::ui_field(no_records_message) %>%
      print()

    return(invisible(NULL))
  }

  # For bulk edit, require confirmation ----
  if (action == "edit") {
    edit_confirm <- usethis::ui_yeah("This will move ALL existing submitted {method} records back to Collecting for editing. Would you like to continue?", yes = "Yes", no = "No", shuffle = FALSE)

    if (!edit_confirm) {
      return(message("Bulk edit stopped."))
    }
  }

  # Show a message that records are being validated/submitted/edited ----

  if (!silent) {
    n_relevant_records <- nrow(relevant_records)
    n_relevant_records_plural <- plural(n_relevant_records)

    usethis::ui_field(
      glue::glue("{n_relevant_records} record{n_relevant_records_plural} being {action_verb(action)}...")
    )
  }

  # Validate/submit/edit records -----
  # For validation, do in batches of 3
  # For submit and edit, do one by one
  batch_size <- dplyr::case_when(
    action %in% c("validate") ~ 3,
    action %in% c("submit", "edit") ~ 1
  )

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
        edit_records(x, project_id, edit_methods_endpoint, token = token)
      }
    }
  ) %>%
    purrr::list_rbind()

  # Summarise results
  statuses <- switch(action,
    "validate" = c("error", "warning", "ok"),
    "submit" = c("ok", "not_ok"),
    "edit" = c("ok", "not_ok")
  )

  action_drop_statuses <- switch(action,
    validate = c("DO NOT DROP"),
    submit = c("not_ok"),
    edit = "not_ok"
  )

  action_res %>%
    summarise_all_statuses(statuses, action, action_drop_statuses)
}

get_collecting_records <- function(project, token = mermaid_token()) {
  # Confirm that they are part of the project first
  in_project <- mermaid_get_me()[["projects"]][[1]] %>%
    dplyr::filter(id == project) %>%
    nrow() == 1

  if (!in_project) {
    stop("You are not a member of this project.", call. = FALSE)
  }

  res <- mermaid_get_project_endpoint(project, "collectrecords")

  # Expand validations, just return the ID, status, and protocol
  res <- res %>%
    tidyr::unpack("validations", names_sep = "_") %>%
    tidyr::unpack("data", names_sep = "_") %>%
    dplyr::select(dplyr::any_of(c("id", "validations_status", "data_protocol")))

  # If there is only one record (or multiple?) and it has not been validated, then "validations" overall is NA -> so the column "validations_status" does not exist, need to create it
  if (!"validations_status" %in% names(res)) {
    res <- res %>%
      dplyr::mutate(validations_status = NA_character_)
  }

  res
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
    # If there is an error in sending the validation request, show the error
    # If there is an error in submitting, then mark it "not_ok" -- an error _is_ a failure to submit the record and should be summarised as such
    if (action == "submit") {
      dplyr::tibble(status = "not_ok")
    } else if (action == "validate") {
      check_errors(response)
    }
  } else {
    # Get the status
    httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      purrr::map_dfr(
        .id = "id",
        \(x) {
          status <- x[["status"]]
          # Just differentiate between ok/not_ok for submit
          if (action == "submit") {
            status <- ifelse(status == "ok", "ok", "not_ok")
          }
          dplyr::tibble(status = status)
        }
      )
  }
}

summarise_all_statuses <- function(df, statuses, action = c("validate", "submit", "action"), action_drop_statuses) {
  status_summary <- df %>%
    dplyr::count(status) %>%
    dplyr::mutate(
      status = forcats::fct_expand(status, statuses),
      status = forcats::fct_relevel(status, statuses)
    ) %>%
    tidyr::complete(status, fill = list(n = 0)) %>%
    split(.$status)

  status_summary %>%
    purrr::walk(\(x) summarise_single_status(x, action, drop = action_drop_statuses))
}

summarise_single_status <- function(df, action, drop) {
  status <- df[["status"]] %>%
    as.character()
  n_status <- df[["n"]]

  # Do not return any messaging if the status is listed in "drop" -- e.g. we do not need to provide a message that 0 records were not successfully submitted/edited, only if there is a problem
  if (status %in% drop & n_status == 0) {
    return(NULL)
  }

  plural <- plural(n_status)

  if (action %in% c("submit", "edit")) {
    message <- dplyr::case_when(
      status == "ok" ~ glue::glue("{n_status} record{plural} successfully {action_verb(action)}."),
      status == "not_ok" ~ glue::glue("{n_status} record{plural} {plural_were(n_status)} not successfully {action_verb(action)}.")
    )
  } else if (action == "validate") {
    message <- dplyr::case_when(
      status %in% c("warning", "error") ~ glue::glue("{n_status} record{plural} produced {status}s in validation"),
      status == "ok" ~ glue::glue("{n_status} record{plural} successfully validated without warnings or errors")
    )
  }

  switch(status,
    "ok" = usethis::ui_done(message),
    "not_ok" = usethis::ui_todo(message),
    "error" = usethis::ui_oops(message),
    "warning" = usethis::ui_todo(message),
  )
}

plural <- function(x) {
  ifelse(x == 1, "", "s")
}

plural_were <- function(x) {
  ifelse(x == 1, "was", "were")
}

action_verb <- function(action) {
  switch(action,
    "validate" = "validated",
    "submit" = "submitted",
    "edit" = "edited and moved back to Collecting",
  )
}
