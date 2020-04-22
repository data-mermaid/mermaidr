#' Get a list of your MERMAID Projects
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
#' \donttest{
#' mermaid_get_my_projects()
#' }
mermaid_get_my_projects <- function(include_test_projects = FALSE, limit = NULL, url = base_url, token = mermaid_token()) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, url = url, token = token)
  } else {
    res <- mermaid_GET("projects", limit = limit, url = url, token = token, status = 90)
  }

  res <- res[["projects"]]

  if (nrow(res) == 0) {
    cols <- mermaid_endpoint_columns[["projects"]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res <- res[, mermaid_endpoint_columns[["projects"]]]
    lookup_choices(res, endpoint = "projects", url = url)
  }
}
