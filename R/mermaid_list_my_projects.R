#' List your MERMAID Projects
#'
#' List MERMAID projects that you have access to. Requires authorization.
#'
#' @inheritParams mermaid_GET
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @return A tibble of MERMAID projects that you have access to, including project name, countries, number of sites, tags, notes, and data policies.
#' @export
#'
#' @examples
#' mermaid_list_my_projects(limit = 5)
mermaid_list_my_projects <- function(limit = 50, include_test_projects = FALSE, url = base_url, token = mermaid_token()) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, url = url, token = token)
  } else {
    res <- mermaid_GET("projects", limit = limit, url = url, token = token, status = 90)
  }

  if (nrow(res) == 0) {
    cols <- mermaid_endpoint_columns[["projects"]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res[, mermaid_endpoint_columns[["projects"]]]
  }
}
