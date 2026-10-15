#' Get MERMAID project managements
#'
#' Get managements for specified MERMAID project(s). Returns metadata on project managements, including ID and name, secondary name, rules, notes, year established, and when the management was created and last updated. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects(include_test_projects = TRUE)
#' projects %>%
#'   mermaid_get_project_managements()
#' }
mermaid_get_project_managements <- function(project = mermaid_get_default_project(), limit = NULL, token = mermaid_token()) {
  get_project_endpoint(project, endpoint = "managements", limit, token)
}
