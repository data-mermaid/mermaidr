#' Search MERMAID projects
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param country Project country
#' @param tag Project tag
#' @inheritParams mermaid_GET
#' @param ...
#'
#' @export
#' @examples
#' mermaid_search_projects(name = "test")
#' mermaid_search_projects(country = "Tanzania")
#' mermaid_search_projects(tag = "WCS Fiji")
#' mermaid_search_projects(country = "Fiji", tag = "WWF-UK")
#'
#' # To search within your projects only:
#' mermaid_search_projects(country = "Fiji", token = mermaid_token())
mermaid_search_projects <- function(name = NULL, country = NULL, tag = NULL, limit = NULL, token = NULL) {
  if (is.null(name) & is.null(country) & is.null(tag)) {
    warning("You haven't provided a `name`, `country`, or `tag` to search by. Just returning ", limit, " projects.", call. = FALSE)
    return(
      mermaid_get_endpoint("projects", name = name, limit = limit, token = token)
    )
  } else if (!is.null(name)) {
    projects <- mermaid_get_endpoint("projects", name = name, token = token)
    if(is.null(country) & is.null(tag)) {
    check_single_project(projects, name)
    }
  } else if (!is.null(country) | !is.null(tag)) {
    if(is.null(token)) {
      projects <- mermaid_list_projects()
    } else {
      projects <- mermaid_list_my_projects(token = token)
    }
  }

  if(!is.null(country)) {
    projects <- projects %>%
      dplyr::filter(grepl(country, countries))
  }
  if(!is.null(tag)) {
    projects <- projects %>%
      dplyr::filter(grepl(tag, tags))
  }

  if(is.null(limit)) {
    projects
  } else {
  head(projects, limit)
  }
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
