#' Search projects by name or ID
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param id Project ID
#'
#' @export
search_projects <- function(name = NULL, id = NULL) {
  if(is.null(name) & is.null(id)) {
    stop("Please supply a `name` or `id` to search by.", call. = FALSE)
  }
  if (!is.null(name)) {
    projects <- get_mermaid_endpoint("projects", name = name)
    check_single_project(projects, name)
  } else if (!is.null(id)) {
    projects <- get_mermaid_endpoint("projects", id = id)
  }

  projects
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
