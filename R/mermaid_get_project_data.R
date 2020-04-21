#' Get MERMAID project data
#'
#' @param method Method to get data for. One of "beltfishes", "benthicpits", or "all" (to get data for both methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or all (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_data(method = "beltfishes", data = "sampleevents", limit = 10)
#'
#' projects %>%
#'   mermaid_get_project_data(method = c("benthicpits", "beltfishes"), data = "sampleevents", limit = 10)
mermaid_get_project_data <- function(project, method = c("beltfishes", "benthicpits"), data = c("observations", "sampleunits", "sampleevents"), limit = NULL, url = base_url, token = mermaid_token(), ...) {
  check_project_data_inputs(method, data)

  endpoint <- construct_endpoint(method, data)

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit, url, token))

  if (all(purrr::map_lgl(res, inherits, "list"))) {
    res <- purrr::map(res, ~ {
      names(.x) <- data
      .x
    })
  }

  if (length(endpoint) == 1) {
    res[[method]]
  } else {
    res
  }
}


check_project_data_inputs <- function(method, data) {
  if (!all(method %in% c("beltfishes", "benthicpits"))) {
    stop('`method` must be one of: "beltfishes", "benthicpits"', call. = FALSE)
  }
  if (!all(data %in% c("observations", "sampleunits", "sampleevents"))) {
    stop('`data` must be one of: "observations", "sampleunits", "sampleevents"', call. = FALSE)
  }
}

# TODO: expand to "all"

construct_endpoint <- function(method, data) {
  method_data <- tidyr::expand_grid(method = method, data = data)

  method_data <- method_data %>%
    dplyr::mutate(data = dplyr::case_when(
      data == "observations" & method == "beltfishes" ~ "obstransectbeltfishes",
      data == "observations" & method == "benthicpits" ~ "obstransectbenthicpits",
      TRUE ~ data
    ))

  method_data %>%
    dplyr::mutate(endpoint = paste0(method, "/", data)) %>%
    split(.$method) %>%
    purrr::map(dplyr::pull, endpoint)
}

project_data_columns <- list(
  `beltfishes/obstransectbeltfishes` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_length", "transect_width", "size_bin", "observers", "depth", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "size_bin", "transect_length", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `beltfishes/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `benthicpits/obstransectbenthicpits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `benthicpits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `benthicpits/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "percent_cover_by_benthic_category_avg", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link")
)
