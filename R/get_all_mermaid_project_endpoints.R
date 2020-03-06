#' Get all endpoints from a specified MERMAID project
#'
#' @inheritParams get_mermaid_project_endpoint
#'
#' @return A list of tibbles, one for each of the MERMAID project endpoints (beltfishtransectmethods, beltfishes, benthiclittransectmethods, benthicpittransectmethods, benthicpits, benthictransects, collectrecords, fishbelttransects, habitatcomplexities, obsbenthiclits, obsbenthicpits, obshabitatcomplexities, obstransectbeltfishs, managements, observers, profiles, project_profiles, sampleevents, sites).
#' @export
#'
#' @examples
#' test_project <- search_projects("Sharla test")
#' get_all_mermaid_project_endpoints(test_project)
get_all_mermaid_project_endpoints <- function(project = get_default_project(), limit = 50, url = base_url, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)

  sapply(names(mermaid_project_endpoint_columns),
    FUN = get_mermaid_project_endpoint,
    project = project, limit = limit, url = url, token = token
  )
}
