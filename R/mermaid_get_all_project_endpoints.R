#' Get all endpoints from a specified MERMAID project
#'
#' @inheritParams mermaid_get_project_endpoint
#'
#' @return A list of tibbles, one for each of the MERMAID project endpoints (beltfishtransectmethods, beltfishes, benthiclittransectmethods, benthicpittransectmethods, benthicpits, benthictransects, collectrecords, fishbelttransects, habitatcomplexities, obsbenthiclits, obsbenthicpits, obshabitatcomplexities, obstransectbeltfishs, managements, observers, project_profiles, sampleevents, sites, beltfishes/obstransectbeltfishes, beltfishes/sampleunits, beltfishes/sampleevents.
#' @export
#'
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
#' mermaid_get_all_project_endpoints(test_project, limit = 1)
#' }
mermaid_get_all_project_endpoints <- function(project = mermaid_get_default_project(), limit = NULL, url = base_url, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)

  res <- purrr::map(mermaid_project_endpoint_columns, mermaid_get_project_endpoint,
    project = project, limit = limit, url = url, token = token
  )

  names(res) <- mermaid_project_endpoint_columns

  res
}
