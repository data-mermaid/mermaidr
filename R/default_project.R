#' Get or set mermaidr MERMAID settings
#'
#' Configure default MERMAID settings
#
#' @param project A way to identify a MERMAID project. Can be a project ID (passed as a character vector directly) or a single project resulting from \code{\link{mermaid_get_endpoint}} or \code{\link{mermaid_search_projects}}.
#'
#' @export
#' @rdname mermaid_settings
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("mermaidr testing")
#' mermaid_set_default_project(test_project)
#' mermaid_get_default_project()
#' mermaid_get_project_endpoint("sites")
#' }
mermaid_set_default_project <- function(project) {
  project_id <- as_id(project)
  Sys.setenv(MERMAIDR_DEFAULT_PROJECT = project_id)
}

#' @rdname mermaid_settings
#' @export
mermaid_get_default_project <- function() {
  Sys.getenv("MERMAIDR_DEFAULT_PROJECT")
}
