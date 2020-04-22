#' Get MERMAID project sites
#'
#' Get sites for specified MERMAID project(s).
#'
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_sites()
mermaid_get_project_sites <- function(project, limit = NULL, url = base_url, token = mermaid_token()) {

  get_project_endpoint(project = project, endpoint = "sites", limit = limit, url = url, token = token)


}

project_sites_columns <- c("id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
