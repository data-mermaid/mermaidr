#' Get MERMAID sites
#'
#' Get a data frame of all MERMAID sites (not project specific).
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \donttest{
#' mermaid_get_sites(limit = 10)
#' }
mermaid_get_sites <- function(limit = NULL, url = base_url) {
  get_endpoint("sites", limit = limit, url = base_url)
}

sites_columns <- c("id", "name", "notes", "project", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
