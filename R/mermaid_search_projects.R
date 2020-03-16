#' Search MERMAID projects
#'
#' Returns metadata on project, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @param name Project name
#' @param country Project country
#' @param tag Project tag
#' @param ...
#'
#' @export
#' @examples
#' mermaid_search_projects(name = "test")
#' mermaid_search_projects(country = "Fiji")
#' mermaid_search_projects(tag = "WCS Fiji")
#' mermaid_search_projects(country = "Fiji", tag = "WWF-UK")
#'
#' # To search within your projects only:
#' mermaid_search_projects(country = "Fiji", token = mermaid_token())
mermaid_search_projects <- function(name = NULL, country = NULL, tag = NULL, limit = 50, ...) {
  if (is.null(name) & is.null(country) & is.null(tag)) {
    warning("You haven't provided a `name`, `country`, or `tag` to search by. Just returning ", limit, " projects.", call. = FALSE)
    return(
      mermaid_list_projects(limit = limit, ...)
    )
  } else if (!is.null(name)) {
    projects <- mermaid_get_endpoint("projects", name = name, ...)
    if(is.null(country) & is.null(tag)) {
    check_single_project(projects, name)
    }
  } else if (!is.null(country) | !is.null(tag)) {
    projects <- mermaid_list_projects(limit = 99999999, ...)
  }

  if(!is.null(country)) {
    projects <- projects %>%
      dplyr::rowwise() %>%
      dplyr::filter(country %in% unlist(countries)) %>%
      dplyr::ungroup()
  }
  if(!is.null(tag)) {
    projects <- projects %>%
      dplyr::rowwise() %>%
      dplyr::filter(tag %in% unlist(tags)) %>%
      dplyr::ungroup()
  }

  head(projects, limit)
}

check_single_project <- function(projects, name) {
  if (nrow(projects) > 1) {
    message(paste0("More than one project with the name '", name, "' exists."))
  }
}
