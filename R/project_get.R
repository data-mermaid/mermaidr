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

#' Get endpoints for a given project.
#'
#' @description
#'
#' These wrapper functions make it easy to get data for a given project.
#' There are 19 functions:
#' \itemize{
#'   \item \code{project_belt_fish_transect_methods()}
#'   \item \code{project_beltfishes()}
#'   \item \code{project_benthic_lit_transect_methods()}
#'   \item \code{project_benthic_pit_transect_methods()}
#'   \item \code{project_benthic_pits()}
#'   \item \code{project_benthic_transects()}
#'   \item \code{project_collect_records()}
#'   \item \code{project_fishbelttransects()}
#'   \item \code{project_habitat_complexities()}
#'   \item \code{project_obs_benthic_lits()}
#'   \item \code{project_obs_benthic_pits()}
#'   \item \code{project_obs_habitat_complexities()}
#'   \item \code{project_obs_transect_beltfishs()}
#'   \item \code{project_managements()}
#'   \item \code{project_observers()}
#'   \item \code{project_profiles()}
#'   \item \code{project_project_profiles()}
#'   \item \code{project_sample_events()}
#'   \item \code{project_sites()}
#'
#'}
#' @inheritParams mermaid_get
#' @param project_id Project ID
#'
#' @export
#' @examples
#' print("todo")
project_belt_fish_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "beltfishtransectmethods",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_beltfishes <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "beltfishes",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_benthic_lit_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "benthiclittransectmethods",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_benthic_pit_transect_methods <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "benthicpittransectmethods",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_benthic_pits <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "benthicpits",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_benthic_transects <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "benthictransects",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_collect_records <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "collectrecords",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_fishbelttransects <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "fishbelttransects",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_habitat_complexities <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "habitatcomplexities",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_obs_benthic_lits <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "obsbenthiclits",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_obs_benthic_pits <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "obsbenthicpits",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_obs_habitat_complexities <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "obshabitatcomplexities",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_obs_transect_beltfishs <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "obstransectbeltfishs",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_managements <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "managements",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_observers <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "observers",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_profiles <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "profiles",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_project_profiles <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "project_profiles",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_sample_events <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "sampleevents",
    limit = limit, results_only = results_only
  )
}

#' @rdname project_belt_fish_transect_methods
#' @export
project_sites <- function(project_id, limit = 50, results_only = TRUE) {
  mermaid_project_get(
    project_id = project_id, endpoint = "sites",
    limit = limit, results_only = results_only
  )
}
