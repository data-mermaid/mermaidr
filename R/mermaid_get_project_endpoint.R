#' Get other MERMAID API project endpoints
#'
#' Get data from MERMAID API project endpoints not covered by other \code{mermaid_get_project_*} functions. Requires authorization.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_endpoint("observers")
#' }
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "benthiclittransectmethods", "benthicpittransectmethods", "benthictransects", "collectrecords", "fishbelttransects", "observers", "project_profiles", "sampleevents"), limit = NULL, token = mermaid_token()) {
  get_project_endpoint(project, endpoint, limit, token)
}

project_other_endpoint_columns <- list(
  beltfishtransectmethods = c("id", "transect", "sample_event", "fishbelt_transect", "observers", "obs_belt_fishes", "created_on", "updated_on"),
  benthiclittransectmethods = c("id", "transect", "sample_event", "benthic_transect", "observers", "obs_benthic_lits", "created_on", "updated_on"),
  benthicpittransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_benthic_pits", "created_on", "updated_on"),
  habitatcomplexitytransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_habitat_complexities", "created_on", "updated_on"),
  bleachingquadratcollectionmethods = c("id", "quadrat", "quadrat_collection", "sample_event", "observers", "obs_colonies_bleached", "created_on", "updated_on"),
  benthicphotoquadrattransectmethods = c("id", "quadrat_transect", "sample_event", "image_classification", "observers", "obs_benthic_photo_quadrats", "created_on", "updated_on"),
  benthictransects = c("id", "sample_time", "sample_event", "number", "label", "current", "depth", "len_surveyed", "notes", "reef_slope", "relative_depth", "tide", "visibility", "created_on", "updated_on"),
  collectrecords = c("id", "profile", "stage", "data", "validations", "created_on", "updated_on"),
  fishbelttransects = c("id", "notes", "number", "len_surveyed", "reef_slope", "width", "size_bin", "sample_event", "created_on", "updated_on"),
  observers = c("id", "profile", "rank", "transectmethod", "created_on", "created_by"),
  project_profiles = c("id", "profile", "is_collector", "is_admin", "role", "created_on", "updated_on"),
  sampleevents = c("id", "data", "sample_date", "notes", "created_by", "site", "management", "created_on", "updated_on")
)
