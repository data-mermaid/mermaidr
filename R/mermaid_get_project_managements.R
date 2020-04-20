#' Get MERMAID project managements
#'
#' Get managements for specified MERMAID project(s).
#'
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
mermaid_get_project_managements <- function(project, limit = NULL, url = base_url) {
  get_project_endpoint(project, endpoint = "managements", limit, url)
}
