#' Get all MERMAID Projects
#'
#' Get all MERMAID projects. Returns metadata on projects, including ID, name, countries, number of sites, tags, notes, status, data sharing policies, and when the project was created and last updated.
#'
#' @inheritParams mermaid_GET
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_projects(limit = 5)
#' }
mermaid_get_projects <- function(include_test_projects = FALSE, limit = NULL, as_is = FALSE) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, as_is = as_is)
  } else {
    res <- mermaid_GET("projects", limit = limit, filter = list(status = 90), as_is = as_is)
  }

  res <- res[["projects"]]
  res <- res[, mermaid_endpoint_columns[["projects"]]]

  if (!as_is) {
    res <- lookup_choices(res, endpoint = "projects")
  }

  res
}

projects_columns <- c("id", "name", "countries", "num_sites", "tags", "notes", "status", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_benthicpqt", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "created_on", "updated_on")
