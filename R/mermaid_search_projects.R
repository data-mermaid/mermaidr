#' Search all MERMAID projects
#'
#' Search all MERMAID projects by project name, country, or tags. Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param countries Project country. Projects are returned if the \code{countries} field contains \code{countries}, not just if it is exactly the same. For a list of countries used in MERMAID, see \code{\link{mermaid_countries}}
#' @param tags Project tags. Projects are returned if the \code{tags} field contains \code{tags}, not just if it is exactly the same.
#' @inheritParams mermaid_GET
#' @inheritParams mermaid_get_my_projects
#'
#' @export
#' @examples
#' \dontrun{
#' mermaid_search_projects(tags = "WCS Fiji")
#' mermaid_search_projects(countries = "Fiji", tags = "WWF-UK")
#'
#' # The countries (or tags) do not have to be exactly the same
#' # A project is returned if it *contains* the countries/tags:
#' mermaid_search_projects(countries = "Tanzania", limit = 1)[["countries"]]
#'
#' # To search within your projects only, use mermaid_search_my_projects():
#' mermaid_search_my_projects(countries = "Fiji")
#' }
mermaid_search_projects <- function(name = NULL, countries = NULL, tags = NULL, include_test_projects = FALSE, limit = NULL) {
  if (is.null(name) & is.null(countries) & is.null(tags)) {
    stop("You haven't provided `name`, `countries`, or `tags` to search by.",
      call. = FALSE
    )
  }

  if (!is.null(name)) {
    if (include_test_projects) {
      projects <- get_endpoint("projects", limit = limit, filter = list(name = name))
    } else {
      projects <- get_endpoint("projects", limit = limit, filter = list(name = name, status = 90))
    }

    if (is.null(countries) & is.null(tags)) {
      check_single_project(projects, name)
    }
  } else if (!is.null(countries) | !is.null(tags)) {
    projects <- mermaid_get_projects(include_test_projects = include_test_projects)
  }

  if (!is.null(countries)) {
    projects <- projects %>%
      dplyr::filter(grepl(!!countries, .data$countries))
  }
  if (!is.null(tags)) {
    projects <- projects %>%
      dplyr::filter(grepl(!!tags, .data$tags))
  }

  if (is.null(limit)) {
    lookup_choices(projects, endpoint = "projects")
  } else {
    head(
      lookup_choices(projects, endpoint = "projects"), limit
    )
  }
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
