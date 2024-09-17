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
mermaid_search_projects <- function(name = NULL, countries = NULL, tags = NULL, include_test_projects = FALSE, limit = NULL, field_report = TRUE) {
  mermaid_search_my_projects(name, countries, tags, include_test_projects, limit, token = NULL, field_report = field_report)
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
