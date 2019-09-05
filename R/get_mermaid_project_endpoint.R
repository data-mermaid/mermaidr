#' Get endpoint from a specified MERMAID project.
#'
#' @inheritParams mermaid_GET
#' @param project A way to identify the project. Can be a project ID (passed as a character vector directly) or a single package resulting from \code{\link{get_mermaid_endpoint}} or \code{\link{search_projects}}.
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
get_mermaid_project_endpoint <- function(project, endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "profiles", "project_profiles", "sampleevents", "sites"), limit = 50, url = base_url, token = mermaid_token()) {
  project_id <- as_id(project)
  endpoint <- check_endpoint(endpoint, mermaid_project_endpoint_columns)
  full_endpoint <- paste0("projects/", project_id, "/", endpoint)
  res <- mermaid_GET(full_endpoint, limit = limit, url = url, token = token)

  if (nrow(res) == 0) {
    cols <- mermaid_project_endpoint_columns[[endpoint]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res[, mermaid_project_endpoint_columns[[endpoint]]]
  }
}

mermaid_project_endpoint_columns <- list(
  beltfishtransectmethods = c("id", "transect", "sample_event", "fishbelt_transect", "observers", "obs_belt_fishes", "created_on", "updated_on"),
  beltfishes = c("id", "transect", "created_on", "updated_on"),
  benthiclittransectmethods = character(0),
  benthicpittransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_benthic_pits", "created_on", "updated_on"),
  benthicpits = c("id", "transect", "interval_size", "created_on", "updated_on"),
  collectrecords = c("id", "project", "profile", "stage", "data", "validations", "created_on", "updated_on"),
  habitatcomplexities = c("id", "transect", "interval_size", "created_on", "updated_on"),
  obsbenthiclits = character(0),
  obsbenthicpits = c("id", "data", "interval", "include", "notes", "benthicpit", "attribute", "growth_form", "created_on", "updated_on"),
  obshabitatcomplexities = c("id", "data", "interval", "include", "notes", "habitatcomplexity", "score", "created_on", "updated_on"),
  obstransectbeltfishs = c("id", "data", "size", "count", "include", "notes", "beltfish", "fish_attribute", "size_bin", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "project", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  observers = c("id", "profile", "profile_name", "rank", "transectmethod", "created_on", "created_by"),
  project_profiles = c("id", "profile", "profile_name", "project", "is_collector", "is_admin", "role", "created_on", "updated_on"),
  sampleevents = c("id", "depth", "data", "sample_date", "sample_time", "notes", "created_by", "site", "management", "visibility", "current", "relative_depth", "tide", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "project", "location", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
)
