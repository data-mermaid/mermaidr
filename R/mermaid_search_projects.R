#' Search MERMAID projects
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param countries Project country. Projects are returned if the \code{countries} field contains \code{countries} not just if it is exactly the same. For a list of countries used in MERMAID, see \code{\link{mermaid_countries}}
#' @param tags Project tags. Projects are returned if the \code{tags} field contains \code{tags}, not just if it is exactly the same.
#' @inheritParams mermaid_GET
#' @inheritParams mermaid_get_my_projects
#'
#' @export
#' @examples
#' \donttest{
#' mermaid_search_projects(tag = "WCS Fiji")
#' mermaid_search_projects(countries = "Fiji", tags = "WWF-UK")
#'
#' # The country (or tag) do not have to be exactly the same
#' # A project is returned if it *contains* the country/tag:
#' mermaid_search_projects(countries = "Tanzania", limit = 1)[["countries"]]
#'
#' # To search within your projects only:
#' mermaid_search_projects(countries = "Fiji", token = mermaid_token())
#'
#' # To include test projects:
#' mermaid_search_projects(name = "test", include_test_projects = TRUE)
#' }
mermaid_search_projects <- function(name = NULL, countries = NULL, tags = NULL, include_test_projects = FALSE, limit = NULL, url = base_url, token = NULL) {
  if (is.null(name) & is.null(country) & is.null(tag)) {
    stop("You haven't provided `name`, `countries`, or `tags` to search by.",
      call. = FALSE
    )
  }

  if (!is.null(name)) {
    if (include_test_projects) {
      projects <- get_endpoint("projects", limit = limit, url = url, token = token, name = name)
    } else {
      projects <- get_endpoint("projects", limit = limit, url = url, token = token, name = name, status = 90)
    }

    if (is.null(countries) & is.null(tags)) {
      check_single_project(projects, name)
    }
  } else if (!is.null(countries) | !is.null(tags)) {
    if (is.null(token)) {
      projects <- mermaid_get_projects(url = url, include_test_projects = include_test_projects)
    } else {
      projects <- mermaid_get_my_projects(url = url, include_test_projects = include_test_projects, token = token)
    }
  }

  if (!is.null(countries)) {
    projects <- projects %>%
      dplyr::filter(grepl(countries, .data$countries))
  }
  if (!is.null(tag)) {
    projects <- projects %>%
      dplyr::filter(grepl(tags, .data$tags))
  }

  if (is.null(limit)) {
    lookup_choices(projects, endpoint = "projects", url = url)
  } else {
    head(
      lookup_choices(projects, endpoint = "projects", url = url), limit
    )
  }
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
