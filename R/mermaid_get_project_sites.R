#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s). Returns metadata on sites, including site ID and name, notes, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @param covariates Deprecated as of version \code{2.0.0}. Please use `mermaidrcovariates` package to access covariates.
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
    cli::cli_alert_info(
      "`covariates` argument is deprecated as of mermaidr version 2.0.0. \\
   Please use the `mermaidrcovariates` package to access covariates. \\
   See the vignette: {.url https://data-mermaid.github.io/mermaidr/articles/covariates_data.html}. \\
   Your results will be returned without any covariates."
    )
  }

  get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token) %>%
    unpack_coordinates()
}
