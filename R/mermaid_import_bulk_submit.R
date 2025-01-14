#' Bulk submit valid records in Collecting
#'
#' Bulk submit validated records in Collecting for a given project. Only submits records that have successfully validated without ANY errors or warnings. To be used after \code{\link{mermaid_import_project_data}} and \code{\link{mermaid_import_bulk_validate}}.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' p <- mermaid_get_my_projects() %>%
#'   head(1)
#'
#' p %>%
#'   mermaid_import_bulk_validate()
#'
#' # 43 records being validated...
#' # ✖ 27 records produced errors in validation
#' # • 13 records produced warnings in validation
#' # ✔ 3 records successfully validated without warnings or errors
#'
#' p %>%
#'   mermaid_import_bulk_submit()
#' }
mermaid_import_bulk_submit <- function(project, token = mermaid_token()) {
  # Show messages
  silent <- FALSE

  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  check_single_project(project_id)

  # Get all records in "collecting"
  # ONLY submit those with validations_status == "ok"
  collect_records <- get_collecting_records(project_id, token)
  valid_collect_records <- collect_records %>%
    dplyr::filter(validations_status == "ok")

  # Handle the case where there are no records to submit
  if (nrow(valid_collect_records) == 0) {
    if (!silent) {
      usethis::ui_field("No valid records in Collecting to submit.") %>%
        print()
    }

    return(invisible(NULL))
  }

  # Go through each and submit, one by one
  # The response returned contains the validation status of each
  # Show a message that records are being validated

  n_submitting <- nrow(valid_collect_records)
  n_submitting_plural <- plural(n_submitting)

  if (!silent) {
    submitting_msg <- glue::glue("{n_submitting} record{n_submitting_plural} being submitted...")
    usethis::ui_field(submitting_msg) %>%
      print()
  }

  # Submit records -----

  if (!silent) {
    progress_bar <- list(format = "{cli::pb_bar} | {cli::pb_percent}") # Show progress bar, but not with ETA -> only % through
  } else {
    progress_bar <- FALSE
  }

  submission_res <- purrr::map(
    valid_collect_records[["id"]],
    .progress = progress_bar,
    \(x) {
      submit_collect_records(x, project_id, token = token)
    }
  ) %>%
    purrr::list_rbind()

  # Summarise results
  submission_res %>%
    dplyr::mutate(status = ifelse(!identical(status, "ok"), "not_ok", "ok")) %>%
    summarise_all_statuses(c("ok", "not_ok"), "submit")
}

submit_collect_records <- function(x, project_id, token = mermaid_token()) {
  url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/submit/"))

  if (length(x) == 1) {
    ids <- list(x)
  } else {
    ids <- x
  }
  validate_body <- list(ids = ids)

  # Post submission
  response <- suppress_http_warning(
    httr::POST(url, encode = "json", body = validate_body, ua, token)
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

summarise_submit_status <- function(df, status) {
  status <- df[["status"]] %>%
    as.character()
  n_status <- df[["n"]]

  plural <- plural(n_status)

  status <- dplyr::case_when(
    status == "ok" ~ glue::glue("{n_message} record{plural} successfully submitted"),
    status == "not_ok" ~ glue::glue("{n_message} record{plural} {plural_were} not successfully submitted")
  )

  switch(status,
    "ok" = usethis::ui_done(status),
    "not_ok" = usethis::ui_todo(status)
  )
}

plural_were <- function(x) {
  ifelse(x == 1, "was", "were")
}
