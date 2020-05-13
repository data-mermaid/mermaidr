#' Search your MERMAID projects
#'
#' Search within your MERMAID projects specifically. Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @inheritParams mermaid_GET
#' @inheritParams mermaid_get_my_projects
#' @inheritParams mermaid_search_projects
#'
#' @export
#' @examples
#' \donttest{
#' mermaid_search_my_projects(tags = "WCS Indonesia")
#' mermaid_search_my_projects(countries = "Indonesia", tags = "Vibrant Oceans")
#'
#' # The country (or tag) do not have to be exactly the same
#' # A project is returned if it *contains* the country/tag:
#' mermaid_search_my_projects(tags = "WCS", limit = 1)[["tags"]]
#' # [1] "WCS Indonesia; Vibrant Oceans"
#' }
mermaid_search_my_projects <- function(name = NULL, countries = NULL, tags = NULL, include_test_projects = FALSE, limit = NULL, url = base_url, token = mermaid_token()) {
  mermaid_search_projects(name = name, countries = countries, tags = tags, include_test_projects = include_test_projects, limit = limit, url = url, token = token)
}
