#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s). Returns metadata on sites, including site ID and name, notes, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated. Optionally get covariates (Allen Coral Atlas, Andrello, Beyer) for sites. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @param covariates Whether to include covariates (Allen Coral Atlas, Andrello, Beyer). Defaults to FALSE.
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
  if (covariates) {
    get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token, filter = list(covars = "true"))
  } else {
    get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token)
  }
}

project_sites_columns <- c(
  "id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure",
  # Covariates
  covars_cols,
  "created_on", "updated_on"
)
