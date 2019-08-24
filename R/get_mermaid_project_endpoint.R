#' Get endpoint from a specified MERMAID project.
#'
#' @inheritParams mermaid_GET
#' @param project_id Project ID
#'
#' @export
#' @examples
#' \dontrun{
#'
#' project_name <- "Beta testing"
#' projects <- get_mermaid_endpoint("projects")
#' project_id <- dplyr::filter(projects, name == project_name)[["id"]]
#' get_mermaid_project_endpoint(project_id, "sampleevents")
#' }
get_mermaid_project_endpoint <- function(project_id, endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "profiles", "project_profiles", "sampleevents", "sites"), limit = 50, url = base_url, token = mermaid_token()) {
  endpoint <- paste0("projects/", project_id, "/", endpoint)
  mermaid_GET(endpoint, limit = limit, url = url, token = mermaid_token())
}
