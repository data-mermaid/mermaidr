#' Get MERMAID project data
#'
#' @param method Method to get data for. One of "beltfishes", "benthicpits", or "all" (to get data for both methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or all (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' projects <- mermaid_get_my_projects(include_test_projects = TRUE)
#' projects %>%
#'   mermaid_get_project_data(method = "beltfishes", data = "sampleevents", limit = 10)
mermaid_get_project_data <- function(project, method = c("beltfishes", "benthicpit"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, url = base_url, token = mermaid_token(), ...) {
  method <- match.arg(method, several.ok = FALSE)
  data <- match.arg(data, several.ok = FALSE)

  endpoint <- construct_endpoint(method, data)

  get_project_endpoint(project, endpoint, limit, url, token)
}

# TODO: expand to "all"

construct_endpoint <- function(method, data) {
  if (data == "observations") {
    data_endpoint <- switch(method,
                            beltfishes = "obstransectbeltfishes",
                            benthicpits = "obstransectbenthicpits")
  } else {
    data_endpoint <- data
  }

  paste0(method, "/", data_endpoint)
}

project_data_columns <- list(
  `beltfishes/obstransectbeltfishes` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_length", "transect_width", "size_bin", "observers", "depth", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
`beltfishes/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "size_bin", "transect_length", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
`beltfishes/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
`benthicpits/obstransectbenthicpits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
`benthicpits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
`benthicpits/sampleevents` =  c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "percent_cover_by_benthic_category_avg", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link")
)
