#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s). Returns metadata on sites, including site ID and name, notes, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated. Optionally, get covariates (Allen Coral Atlas, Andrello, Beyer) for sites. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @covariates Whether to include covariates (Allen Coral Atlas, Andrello, Beyer). Defaults to FALSE.
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

project_sites_columns <- c(
  "id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure",
  # Covariates
  "aca_geomorphic", "aca_benthic", "andrello_grav_nc", "andrello_sediment", "andrello_nutrient", "andrello_pop_count", "andrello_num_ports", "andrello_reef_value", "andrello_cumul_score", "beyer_score", "beyer_scorecn", "beyer_scorecy", "beyer_scorepfc", "beyer_scoreth", "beyer_scoretr",

  "created_on", "updated_on"
)
