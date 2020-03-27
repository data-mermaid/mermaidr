#' Get or set mermaidr MERMAID settings
#'
#' Configure default MERMAID settings
#
#' @param project A way to identify a MERMAID project. Can be project IDs (passed as a character vector directly) or projects resulting from \code{\link{mermaid_get_endpoint}} or \code{\link{mermaid_search_projects}}.
#'
#' @export
#' @rdname mermaid_settings
#' @examples
#' test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
#' mermaid_set_default_project(test_project)
#' mermaid_get_default_project()
#' \dontrun{
#' mermaid_get_project_endpoint(endpoint = "sites")
#' }
mermaid_set_default_project <- function(project) {
  project_id <- as_id(project)

  if(length(project_id) > 1) {
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
