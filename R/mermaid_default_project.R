#' Get or set mermaidr MERMAID projects
#'
#' Configure default MERMAID projects. \code{mermaid_set_default_project} sets project(s) as the default to be used automatically in \code{mermaid_get_project_*} functions. \code{mermaid_get_default_project} lists the default projects.
#
#' @param project A way to identify MERMAID project(s). Can be project IDs (passed as a character vector directly) or projects resulting from \code{\link{mermaid_get_my_projects}} or \code{\link{mermaid_search_my_projects}}.
#'
#' @export
#' @rdname mermaid_settings
#' @examples
#' \donttest{
#' test_project <- mermaid_search_my_projects("Sharla test", include_test_projects = TRUE)
#' mermaid_set_default_project(test_project)
#' mermaid_get_default_project()
#' # Since a default project is set, you can use mermaid_get_project_endpoint
#' # without explicitly supplying a project
#' mermaid_get_project_sites()
#' }
mermaid_set_default_project <- function(project) {
  project_id <- as_id(project)

  if (length(project_id) > 1) {
    project_ids <- paste0(project_id, collapse = ",")
    Sys.setenv(MERMAIDR_DEFAULT_PROJECT = project_ids)
  } else {
    Sys.setenv(MERMAIDR_DEFAULT_PROJECT = project_id)
  }
}

#' @rdname mermaid_settings
#' @export
mermaid_get_default_project <- function() {
  project_id <- Sys.getenv("MERMAIDR_DEFAULT_PROJECT")
  if (grepl(",", project_id)) {
    strsplit(project_id, ",")[[1]]
  } else {
    project_id
  }
}
