#' Get your MERMAID Projects
#'
#' Get MERMAID projects you have access to. Returns metadata on projects, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated. Requires authorization.
#'
#' @inheritParams mermaid_GET
#' @inheritParams get_project_endpoint
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @return A tibble of MERMAID projects that you have access to, including project name, countries, number of sites, tags, notes, and data policies.
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_my_projects()
#' }
mermaid_get_my_projects <- function(include_test_projects = FALSE, limit = NULL, token = mermaid_token(), as_is = FALSE) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, token = token, as_is = as_is)
  } else {
    res <- mermaid_GET("projects", limit = limit, token = token, filter = list(status = 90), as_is = as_is)
  }

  res <- res[["projects"]]

  if (!as_is) {
    if (nrow(res) == 0) {
      cols <- mermaid_endpoint_columns[["projects"]]
      res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
      names(res) <- cols
      res
    } else {
      res <- res[, mermaid_endpoint_columns[["projects"]]]
      lookup_choices(res, endpoint = "projects")
    }
  } else {
    res
    # NOTE - if 0 projects, then as_is actually just returns a 0x0 tibble
    # Which is why as_is constructs the columns above - but should be fine for non-field report, since it very much is "as is"
  }
}
