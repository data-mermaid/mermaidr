#' Bulk validate records in Collecting
#'
#' Bulk validates records in Collecting for a given project, and returns information on how many records produced errors, produced warnings, or were successfully validated without errors or warnings. To be used after \code{\link{mermaid_import_project_data}}.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_import_bulk_validate()
#'
#' # 43 records being validated...
#' # ✖ 27 records produced errors in validation
#' # • 13 records produced warnings in validation
#' # ✔ 3 records successfully validated without warnings or errors
#' }
mermaid_import_bulk_validate <- function(project, token = mermaid_token()) {
  # Show messages
  silent <- FALSE

  # One project at a time
  project_id <- as_id(project)
  check_project(project_id)

  check_single_project(project_id)

  # Get all records in "collecting"
  # Validate ALL, even those that already have warning/error, so that summary is accurate to everything in collecting
  collect_records <- get_collecting_records(project_id, token)

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
  n_validating_plural <- plural(n_validating)

  if (!silent) {
    validating_msg <- glue::glue("{n_validating} record{n_validating_plural} being validated...")
    usethis::ui_field(validating_msg) %>%
      print()
  }

  # Validate records -----

  # Do in batches of three
  batch_size <- 3
  collect_records_split <- collect_records %>%
    dplyr::mutate(...validate_group = ceiling(dplyr::row_number() / batch_size)) %>%
    split(.$...validate_group)

  if (!silent) {
    progress_bar <- list(format = "{cli::pb_bar} | {cli::pb_percent}") # Show progress bar, but not with ETA -> only % through
  } else {
    progress_bar <- FALSE
  }

  validation_res <- purrr::map(
    collect_records_split,
    .progress = progress_bar,
    \(x) {
      validate_collect_records(x, project_id, token = token)
    }
  ) %>%
    purrr::list_rbind()

  # Summarise results
  validation_statuses <- c("error", "warning", "ok")

  validation_summary <- validation_res %>%
    dplyr::count(status) %>%
    dplyr::mutate(
      status = forcats::fct_expand(status, validation_statuses),
      status = forcats::fct_relevel(status, validation_statuses)
    ) %>%
    tidyr::complete(status, fill = list(n = 0))

  validation_summary %>%
    split(.$status) %>%
    purrr::walk(summarise_validations_status)
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

validate_collect_records <- function(x, project_id, token = mermaid_token()) {
  url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/collectrecords/validate/"))

  if (nrow(x) == 1) {
    ids <- list(x[["id"]])
  } else {
    ids <- x[["id"]]
  }
  validate_body <- list(ids = ids)

  # Post validation
  response <- suppress_http_warning(
    httr::POST(url, encode = "json", body = validate_body, ua, token)
  )

  if (httr::http_error(response)) {
    # If an actual error in sending the request, not the validation itself
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

summarise_all_validations_statuses <- function(df, statuses = c("error", "warning", "ok")) {
  validation_summary <- df %>%
    dplyr::count(status) %>%
    dplyr::mutate(
      status = forcats::fct_expand(status, statuses),
      status = forcats::fct_relevel(status, statuses)
    ) %>%
    tidyr::complete(status, fill = list(n = 0))

  validation_summary %>%
    split(.$status) %>%
    purrr::walk(summarise_validations_status)
}

summarise_validations_status <- function(df) {
  status <- df[["status"]] %>%
    as.character()
  n_status <- df[["n"]]

  plural <- plural(n_status)

  message <- dplyr::case_when(
    status %in% c("warning", "error") ~ glue::glue("{n_status} record{plural} produced {status}s in validation"),
    status == "ok" ~ glue::glue("{n_status} record{plural} successfully validated without warnings or errors")
  )

  switch(status,
    "error" = usethis::ui_oops(message),
    "warning" = usethis::ui_todo(message),
    "ok" = usethis::ui_done(message)
  )
}

plural <- function(x) {
  ifelse(x == 1, "", "s")
}
