#' List MERMAID Projects
#'
#' @inheritParams mermaid_GET
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @return A tibble of MERMAID projects including project name, countries, number of sites, tags, notes, and data policies.
#' @export
#'
#' @examples
#' mermaid_list_projects(limit = 5)
mermaid_list_projects <- function(include_test_projects = FALSE, limit = NULL, url = base_url) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, url = url)
  } else {
    res <- mermaid_GET("projects", limit = limit, url = url, status = 90)
  }

  res[, mermaid_endpoint_columns[["projects"]]]
}
