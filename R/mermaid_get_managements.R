#' Get MERMAID managements
#'
#' Get a data frame of all MERMAID managements (not project specific).
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_managements(limit = 10)
#' }
mermaid_get_managements <- function(limit = NULL, url = base_url) {
  get_endpoint("managements", limit = limit, url = base_url)
}

managements_columns <- c("id", "name", "name_secondary", "rules", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on")
