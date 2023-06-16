#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s). Returns metadata on sites, including site ID and name, notes, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_sites()
#' }
mermaid_get_project_sites <- function(project = mermaid_get_default_project(), limit = NULL, token = mermaid_token(), covariates = FALSE) {
  get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token, filter = list(covars = covariates))
}

project_sites_columns <- c("id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "created_on", "updated_on")
