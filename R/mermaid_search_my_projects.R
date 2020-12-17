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
#' \dontrun{
#' mermaid_search_my_projects(tags = "WCS Indonesia")
#' mermaid_search_my_projects(countries = "Indonesia", tags = "Vibrant Oceans")
#'
#' # The country (or tag) do not have to be exactly the same
#' # A project is returned if it *contains* the country/tag:
#' mermaid_search_my_projects(tags = "WCS", limit = 1)[["tags"]]
#' # [1] "WCS Indonesia; Vibrant Oceans"
#'
#' # To search all projects (not just yours), use mermaid_search_projects():
#' mermaid_search_projects(countries = "Fiji")
#' }
mermaid_search_my_projects <- function(name = NULL, countries = NULL, tags = NULL, include_test_projects = FALSE, limit = NULL, token = mermaid_token()) {
    if (is.null(name) & is.null(countries) & is.null(tags)) {
      stop("You haven't provided `name`, `countries`, or `tags` to search by.",
           call. = FALSE
      )
    }

    if (!is.null(name)) {
      if (include_test_projects) {
        projects <- get_endpoint("projects", limit = limit, token = token, name = name)
      } else {
        projects <- get_endpoint("projects", limit = limit, token = token, name = name, status = 90)
      }

      if (is.null(countries) & is.null(tags)) {
        check_single_project(projects, name)
      }
    } else if (!is.null(countries) | !is.null(tags)) {
        projects <- mermaid_get_my_projects(include_test_projects = include_test_projects, token = token)
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
