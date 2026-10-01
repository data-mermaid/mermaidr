#' Get all MERMAID Projects
#'
#' Get all MERMAID projects. Returns metadata on projects, including ID, name,
#'  countries, number of sites, tags, project admins, notes, status, data
#'  sharing policies, and when the project was created and last updated.
#'
#' @inheritParams mermaid_GET
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_projects(limit = 5)
#' }
mermaid_get_projects <- function(include_test_projects = FALSE, limit = NULL) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit)
  } else {
    res <- mermaid_GET("projects", limit = limit, filter = list(status = 90))
  }

  res <- res[["projects"]]
  remove_blacklist_endpoint_columns(res, "projects")
}
