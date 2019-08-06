#' Get endpoint from a specified MERMAID project.
#'
#' @inheritParams mermaid_get
#' @param project_id Project ID
#' @param endpoint API endpoint
#'
#' @export
#' @examples
#' \dontrun{
#'
#' project_name <- "Beta testing"
#' projects <- mermaid_projects()
#' project_id <- dplyr::filter(projects, name == project_name)[["id"]]
#' project_get(project_id, "sampleevents")
#' }
project_get <- function(project_id, endpoint, limit = 50, results_only = TRUE) {
  endpoint <- paste0("projects/", project_id, "/", endpoint)
  mermaid_get(endpoint)
}

# Assorted wrappers for project endpoints

#' Get belt fish transect methods for a given project.
#'
#' @inheritParams mermaid_get
#' @param project_id Project ID
#' @family project endpoints
#' @export
get_project_belt_fish_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "beltfishtransectmethods",
    limit = limit, results_only = results_only
  )
}

#' Get beltfishes for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_beltfishes <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "beltfishes",
    limit = limit, results_only = results_only
  )
}

#' Get benthic lit transect methods for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_benthic_lit_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "benthiclittransectmethods",
    limit = limit, results_only = results_only
  )
}

#' Get benthic pit transect methods for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_benthic_pit_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "benthicpittransectmethods",
    limit = limit, results_only = results_only
  )
}

#' Get benthic pits for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_benthic_pits <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "benthicpits",
    limit = limit, results_only = results_only
  )
}

#' Get benthic transects for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_benthic_transects <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "benthictransects",
    limit = limit, results_only = results_only
  )
}

#' Get collect records for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_collect_records <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "collectrecords",
    limit = limit, results_only = results_only
  )
}

#' Get fishbelt transects for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_fishbelt_transects <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "fishbelttransects",
    limit = limit, results_only = results_only
  )
}

#' Get habitat complexities for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_habitat_complexities <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "habitatcomplexities",
    limit = limit, results_only = results_only
  )
}

#' Get obs benthic lits for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_obs_benthic_lits <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "obsbenthiclits",
    limit = limit, results_only = results_only
  )
}

#' Get obs benthic pits for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_obs_benthic_pits <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "obsbenthicpits",
    limit = limit, results_only = results_only
  )
}

#' Get obs habitat complexities for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_obs_habitat_complexities <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "obshabitatcomplexities",
    limit = limit, results_only = results_only
  )
}

#' Get obs transect beltfishes for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_obs_transect_beltfishs <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "obstransectbeltfishs",
    limit = limit, results_only = results_only
  )
}

#' Get managements for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_managements <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "managements",
    limit = limit, results_only = results_only
  )
}

#' Get observers for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_observers <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "observers",
    limit = limit, results_only = results_only
  )
}

#' Get profiles for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_profiles <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "profiles",
    limit = limit, results_only = results_only
  )
}

#' Get project profiles for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_project_profiles <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "project_profiles",
    limit = limit, results_only = results_only
  )
}

#' Get sample events for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_sample_events <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "sampleevents",
    limit = limit, results_only = results_only
  )
}

#' Get sites for a given project.
#'
#' @inheritParams get_project_belt_fish_transect_methods
#' @family project endpoints
#' @export
get_project_sites <- function(project_id, limit = 50, results_only = TRUE) {
  project_get(
    project_id = project_id, endpoint = "sites",
    limit = limit, results_only = results_only
  )
}
