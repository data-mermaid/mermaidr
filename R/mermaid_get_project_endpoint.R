#' Get other MERMAID API project endpoints
#'
#' @inheritParams get_project_endpoint
#'
#' @return
#' @export
#'
#' @examples
mermaid_get_project_endpoint <- function(project, endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "observers", "project_profiles", "sampleevents"), limit = NULL, url = base_url, token = mermaid_token()) {
  get_project_endpoint(project, endpoint, limit, base_url, token)
}
