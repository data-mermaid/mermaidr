#' Get or set mermaidr MERMAID settings
#'
#' Configure default MERMAID settings
#
#' @param project A way to identify a MERMAID project. Can be a project ID (passed as a character vector directly) or a single project resulting from \code{\link{get_mermaid_endpoint}} or \code{\link{search_projects}}.
#'
#' @export
#' @rdname mermaid_settings
#' @examples \dontrun{
#' test_project <- search_projects("mermaidr testing", exact_name = TRUE)
#' set_default_project(test_project)
#' get_default_project()
#' get_mermaid_project_endpoint(endpoint = "sites")
#' }
set_default_project <- function (project) {
  project_id <- as_id(project)
  Sys.setenv(MERMAIDR_DEFAULT_PROJECT = project_id)
}

#' @rdname mermaid_settings
#' @export
get_default_project <- function () {
  Sys.getenv("MERMAIDR_DEFAULT_PROJECT")
}
