#' Get MERMAID sites
#'
#' Get all MERMAID sites (not project specific). Returns metadata on sites, including site ID and name, notes, project, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated.
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_sites(limit = 10)
#' }
mermaid_get_sites <- function(limit = NULL, token = mermaid_token(), field_report = TRUE) {
  get_endpoint("sites", limit = limit, token = token, field_report = field_report)
}

sites_columns <- c("id", "name", "notes", "project", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
