#' Get MERMAID project managements
#'
#' Get managements for specified MERMAID project(s).
#'
#' @inheritParams get_project_endpoint
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

project_managements_columns <- c("id", "name", "name_secondary", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on")
