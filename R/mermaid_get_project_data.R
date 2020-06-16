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
#' \dontrun{
#' # projects <- mermaid_get_my_projects()
#' # projects %>%
#'   mermaid_get_project_data(method = "fishbelt", data = "observations", limit = 10)
#'
#' # projects %>%
#' #  mermaid_get_project_data(method = c("benthicpit", "fishbelt"), data = "sampleevents", limit = 10)
#' }
mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "habitatcomplexity", "bleaching", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, url = base_url, token = mermaid_token()) {
  check_project_data_inputs(method, data)

  if (any(method == "all")) {
    method <- c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching")
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

  if (any(method == "bleaching")) {
    if (all(data == "observations")) {
      names(res[["bleachingqcs"]]) <- c("colonies_bleached", "percent_cover")
    } else if ("observations" %in% data) {
      names(res[["bleachingqcs"]]) <- sub_one_for_many(x = data, pattern = "observations", replacement = c("colonies_bleached", "percent_cover"))
      res[["bleachingqcs"]][["observations"]] <- list(colonies_bleached = res[["bleachingqcs"]][["colonies_bleached"]], percent_cover = res[["bleachingqcs"]][["percent_cover"]])
      res[["bleachingqcs"]] <- res[["bleachingqcs"]][data]
    }
  }

  if (length(endpoint) == 1) {
    res[[1]]
  } else {
    names(res) <- method
    res
  }
}


check_project_data_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching", "all"))) {
    stop('`method` must be one of: "fishbelt", "benthicpit", "benthiclit", "habitatcomplexity", "bleaching", "all"', call. = FALSE)
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
        .data$method == "benthicpit" ~ "benthicpits",
        .data$method == "benthiclit" ~ "benthiclits",
        .data$method == "habitatcomplexity" ~ "habitatcomplexities",
        .data$method == "bleaching" ~ "bleachingqcs"
      ),
      data = dplyr::case_when(
        .data$data == "observations" & .data$method == "beltfishes" ~ "obstransectbeltfishes",
        .data$data == "observations" & .data$method == "benthicpits" ~ "obstransectbenthicpits",
        .data$data == "observations" & .data$method == "benthiclits" ~ "obstransectbenthiclits",
        .data$data == "observations" & .data$method == "habitatcomplexities" ~ "obshabitatcomplexities",
        .data$data == "observations" & .data$method == "bleachingqcs" ~ "obscoloniesbleacheds,obsquadratbenthicpercents",
        TRUE ~ data
      )
    ) %>%
    tidyr::separate_rows(method, data, sep = ",")

  method_data <- method_data %>%
    dplyr::mutate(endpoint = paste0(.data$method, "/", .data$data))

  method_data_list <- method_data %>%
    split(method_data$method) %>%
    purrr::map(dplyr::pull, .data$endpoint)

  method_data_list[unique(method_data[["method"]])]
}

project_data_columns <- list(
  `beltfishes/obstransectbeltfishes` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_length", "transect_width", "size_bin", "observers", "depth", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "count", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "size_bin", "transect_length", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `beltfishes/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `benthicpits/obstransectbenthicpits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `benthicpits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `benthicpits/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "percent_cover_by_benthic_category_avg", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `benthiclits/obstransectbenthiclits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "label", "observers", "benthic_category", "benthic_attribute", "growth_form", "length", "data_policy_benthiclit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `benthiclits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "observers", "percent_cover_by_benthic_category", "data_policy_benthiclit", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `benthiclits/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "percent_cover_by_benthic_category_avg", "data_policy_benthiclit", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `habitatcomplexities/obshabitatcomplexities` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "label", "observers", "interval", "score", "data_policy_habitatcomplexity", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `habitatcomplexities/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "observers", "score_avg", "data_policy_habitatcomplexity", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `habitatcomplexities/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "score_avg_avg", "data_policy_habitatcomplexity", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `bleachingqcs/obscoloniesbleacheds` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "quadrat_size", "label", "observers", "benthic_attribute", "growth_form", "count_normal", "count_pale", "count_20", "count_50", "count_80", "count_100", "count_dead", "data_policy_bleachingqc", "project_notes", "site_notes", "management_notes", "contact_link"),
  `bleachingqcs/obsquadratbenthicpercents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "quadrat_size", "label", "observers", "quadrat_number", "percent_hard", "percent_soft", "percent_algae", "data_policy_bleachingqc", "project_notes", "site_notes", "management_notes", "contact_link"),
  `bleachingqcs/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "quadrat_size", "count_total", "count_genera", "percent_normal", "percent_pale", "percent_bleached", "quadrat_count", "percent_hard_avg", "percent_soft_avg", "percent_algae_avg", "data_policy_bleachingqc", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `bleachingqcs/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "quadrat_size_avg", "count_total_avg", "count_genera_avg", "percent_normal_avg", "percent_pale_avg", "percent_bleached_avg", "quadrat_count_avg", "percent_hard_avg_avg", "percent_soft_avg_avg", "percent_algae_avg_avg", "data_policy_bleachingqc", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"))
