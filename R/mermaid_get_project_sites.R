#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s).
#'
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_sites()
#' }
mermaid_get_project_sites <- function(project = mermaid_get_default_project(), limit = NULL, token = mermaid_token()) {
  get_project_endpoint(project = project, endpoint = "sites", limit = limit, token = token)
}

project_sites_columns <- c("id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
