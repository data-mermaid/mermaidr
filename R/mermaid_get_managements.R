#' Get MERMAID managements
#'
#' Get all MERMAID managements (not project specific). Returns metadata on managements, including ID and name, secondary name, rules, notes, year established, and when the management was created and last updated.
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_managements(limit = 10)
#' }
mermaid_get_managements <- function(limit = NULL, token = mermaid_token()) {
  get_endpoint("managements", limit = limit, token = token)
}

managements_columns <- c("id", "name", "name_secondary", "rules", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on")
