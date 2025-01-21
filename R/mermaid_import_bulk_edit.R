#' Bulk edit submitted records, moving them back into Collecting
#'
#' Returns submitted records back to Collecting, where they can be edited, for a given project and method. This should only be used when errors in data are discovered and ALL records need to be moved back to Collecting. To be used after \code{\link{mermaid_import_project_data}}, \code{\link{mermaid_import_bulk_validate}}, and \code{\link{mermaid_import_bulk_submit}}.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @param method Method to return submitted data back into "editing" state for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", bleaching", or "habitatcomplexity".
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
mermaid_import_bulk_edit <- function(project, method = NULL, token = mermaid_token()) {
  import_bulk_action(project, action = "edit", method = method, token = token)
}

edit_records <- function(x, project_id, methods_endpoint, token = mermaid_token()) {
  su_id <- x[["id"]]
  url <- httr::modify_url(base_url, path = glue::glue("v1/projects/{project_id}/{methods_endpoint}/{su_id}/edit/"))

  # Post submission
  response <- suppress_http_warning(
    httr::PUT(url, encode = "json", ua, token)
  )

  if (httr::http_error(response)) {
    # Handle this as an error in "status" for edit -- for validation, it makes sense to return an error in the API as an error
    # But for edit, an error *is* failure to edit the record and should be summarised as such
    dplyr::tibble(status = "not_ok")
  } else {
    # Get the status
    httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      purrr::map_dfr(
        .id = "id",
        # What returns is the new collect record ID, no status etc
        \(x) {
          # Query the collect records and check that the new ID is one of them
          collect_records <- get_collecting_records(project_id, token = token)
          dplyr::tibble(status = ifelse(x %in% collect_records[["id"]], "ok", "not_ok"))
        }
      )
  }
}

method_to_methods_endpoint <- function(method) {
  methods_endpoint_names[[method]]
}

methods_endpoint_names <- list(
  fishbelt = "beltfishtransectmethods",
  benthiclit = "benthiclittransectmethods",
  benthicpit = "benthicpittransectmethods",
  benthicpqt = "benthicphotoquadrattransectmethods",
  bleaching = "bleachingquadratcollectionmethods",
  habitatcomplexity = "habitatcomplexitytransectmethods"
)

protocol_to_endpoint_names <- function(method) {
  protocol_methods_endpoint_names[[method]]
}

protocol_methods_endpoint_names <- list(
  beltfishes = "beltfishtransectmethods",
  benthiclits = "benthiclittransectmethods",
  benthicpits = "benthicpittransectmethods",
  benthicpqts = "benthicphotoquadrattransectmethods",
  bleachingqcs = "bleachingquadratcollectionmethods",
  habitatcomplexities = "habitatcomplexitytransectmethods"
)
