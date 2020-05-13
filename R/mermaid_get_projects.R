#' Get a list of all MERMAID Projects
#'
#' @inheritParams mermaid_GET
#' @param include_test_projects Whether to include test projects. Defaults to FALSE.
#'
#' @return A tibble of MERMAID projects including project name, countries, number of sites, tags, notes, and data policies.
#' @export
#'
#' @examples
#' \donttest{
#' mermaid_get_projects(limit = 5)
#' }
mermaid_get_projects <- function(include_test_projects = FALSE, limit = NULL, url = base_url) {
  if (include_test_projects) {
    res <- mermaid_GET("projects", limit = limit, url = url)
  } else {
    res <- mermaid_GET("projects", limit = limit, url = url, status = 90)
  }

  res <- res[["projects"]]
  res <- res[, mermaid_endpoint_columns[["projects"]]]
  lookup_choices(res, endpoint = "projects", url = url)
}

projects_columns <- c("id", "name", "countries", "num_sites", "tags", "notes", "status", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "created_on", "updated_on")
