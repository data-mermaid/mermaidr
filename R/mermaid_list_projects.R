#' List MERMAID Projects
#'
#' @inheritParams mermaid_GET
#'
#' @return A tibble of MERMAID projects including project name, countries, number of sites, tags, notes, and data policies.
#' @export
#'
#' @examples
#' mermaid_list_projects(limit = 5)
mermaid_list_projects <- function(limit = 50, url = base_url) {
  res <- mermaid_GET("projects", limit = limit, url = url)

  res[, mermaid_endpoint_columns[["projects"]]]
}
