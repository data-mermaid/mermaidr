#' Search projects by name or ID
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param id Project ID
#' @param exact_name Whether the search should be by the exact \code{name} only (as opposed to the package name just needing to contain \code{name}). Defaults to FALSE, and is not used if \code{id} is supplied.
#'
#' @export
search_projects <- function(name = NULL, id = NULL, exact_name = FALSE) {
  projects <- get_mermaid_endpoint("projects", limit = 99999)
  if (!is.null(id)) {
    projects[projects[["id"]] == id, ]
  } else if (!is.null(name)) {
    if (exact_name) {
      projects[tolower(name) == tolower(projects[["name"]]), ]
    } else {
      projects[grepl(tolower(name), tolower(projects[["name"]])), ]
    }
  }
}
