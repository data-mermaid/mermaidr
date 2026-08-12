#' Get other MERMAID API project endpoints
#'
#' Get data from MERMAID API project endpoints not covered by other \code{mermaid_get_project_*} functions. Requires authorization.
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
#'   mermaid_get_project_endpoint("observers")
#' }
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "benthiclittransectmethods", "benthicpittransectmethods", "benthictransects", "collectrecords", "fishbelttransects", "observers", "project_profiles", "sampleevents"), limit = NULL, token = mermaid_token()) {
  get_project_endpoint(project, endpoint, limit, token)
}
