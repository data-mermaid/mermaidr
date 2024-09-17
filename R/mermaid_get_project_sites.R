#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s). Returns metadata on sites, including site ID and name, notes, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated. Optionally get covariates. Requires authorization.
#'
#' The included covariates are: geomorphic zonation and benthic habitat from the \href{https://allencoralatlas.org}{Allen Coral Atlas}; market gravity, water pollution (sediments and nitrogen), coastal population, industrial development (number of ports), tourism (reef value), and a cumulative pressure index from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12858}{\emph{A global map of human pressures on tropical coral reefs}} by Andrello et al., 2022; scores from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12587}{\emph{Risk-sensitive planning for conserving coral reefs under rapid climate change}} by Beyer et al., 2018.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @param covariates Whether to include covariates. Defaults to FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_sites()
#' }
mermaid_get_project_sites <- function(project = mermaid_get_default_project(), limit = NULL, token = mermaid_token(), covariates = FALSE, field_report = TRUE) {
  if (covariates) {
    get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token, filter = list(covars = "true"), field_report = field_report)
  } else {
    get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token, field_report = field_report)
  }
}

project_sites_columns <- c(
  "id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure",
  # Covariates
  covars_cols,
  "created_on", "updated_on"
)
