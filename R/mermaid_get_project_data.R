#' Get MERMAID project data
#'
#' Get fishbelt or benthic PIT data for your MERMAID projects. Data is available at the observation, sample unit, and sample event level.
#'
#' Fish belt method data is available by setting \code{method} to "fishbelt". Fish belt observations data contains individual observations recorded in MERMAID, while sample units contains total biomass in kg/ha per sample unit, by trophic group. Sample events data contains \emph{mean} total biomass in kg/ha per sample event and by trophic group.
#'
#' Benthic PIT method data is available by setting \code{method} to "benthicpit". Similarly, benthic PIT observations contain individual observations. Sample units data returns percent cover per sample unit, by benthic category. Sample events contain \emph{mean} percent cover per sample event, by benthic category.
#'
#' @param method Method to get data for. One of "fishbelt", "benthicpit", or "all" (to get data for both methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or all (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \donttest{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_data(method = "fishbelt", data = "observations", limit = 10)
#'
#' projects %>%
#'   mermaid_get_project_data(method = c("benthicpit", "fishbelt"), data = "sampleevents", limit = 10)
#' }
mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthicpit", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, url = base_url, token = mermaid_token()) {
  check_project_data_inputs(method, data)

  if (any(method == "all")) {
    method <- c("fishbelt", "benthicpit")
  }
  if (any(data == "all")) {
    data <- c("observations", "sampleunits", "sampleevents")
  }

  endpoint <- construct_endpoint(method, data)

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit, url, token))

  if (all(purrr::map_lgl(res, inherits, "list"))) {
    res <- purrr::map(res, ~ {
      names(.x) <- data
      .x
    })
  }

  if (length(endpoint) == 1) {
    res[[1]]
  } else {
    names(res) <- method
    res
  }
}


check_project_data_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "all"))) {
    stop('`method` must be one of: "fishbelt", "benthicpit", "all"', call. = FALSE)
  }
  if (!all(data %in% c("observations", "sampleunits", "sampleevents", "all"))) {
    stop('`data` must be one of: "observations", "sampleunits", "sampleevents", "all"', call. = FALSE)
  }
}

construct_endpoint <- function(method, data) {
  method_data <- tidyr::expand_grid(method = method, data = data)

  method_data <- method_data %>%
    dplyr::mutate(
      method = dplyr::case_when(
        .data$method == "fishbelt" ~ "beltfishes",
        .data$method == "benthicpit" ~ "benthicpits"
      ),
      data = dplyr::case_when(
        .data$data == "observations" & .data$method == "beltfishes" ~ "obstransectbeltfishes",
        .data$data == "observations" & .data$method == "benthicpits" ~ "obstransectbenthicpits",
        TRUE ~ data
      )
    )

  method_data <- method_data %>%
    dplyr::mutate(endpoint = paste0(.data$method, "/", .data$data))

  method_data %>%
    split(method_data$method) %>%
    purrr::map(dplyr::pull, .data$endpoint)
}

project_data_columns <- list(
  `beltfishes/obstransectbeltfishes` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_length", "transect_width", "size_bin", "observers", "depth", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "size_bin", "transect_length", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `beltfishes/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `benthicpits/obstransectbenthicpits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `benthicpits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `benthicpits/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "percent_cover_by_benthic_category_avg", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link")
)
