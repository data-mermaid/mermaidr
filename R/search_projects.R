#' Search projects by name or ID
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param id Project ID
#'
#' @export
#'
#' @examples
#' \dontrun {
#' search_projects(name = "mermaidr testing")
#' }
search_projects <- function(name = NULL, id = NULL, fixed = FALSE) {
  projects <- get_mermaid_endpoint("projects", limit = 99999)
  if (!is.null(id)) {
    projects[projects[["id"]] == id, ]
  } else if (!is.null(name)) {
    if (fixed) {
      projects[tolower(name) == tolower(projects[["name"]]), ]
    } else {
      projects[grepl(tolower(name), tolower(projects[["name"]])), ]
    }
  }
}
