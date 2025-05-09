#' Get MERMAID project data
#'
#' Get Fish Belt, Benthic LIT, Benthic PIT, Bleaching, or Habitat Complexity data for your MERMAID projects. Data is available at the observation, sample unit, and sample event level. Optionally get covariates at the site level. See Details section for more. Requires authorization.
#'
#' Fish Belt method data is available by setting \code{method} to "fishbelt". Fish Belt observations data contains individual observations recorded in MERMAID, while sample units contains total biomass in kg/ha per sample unit, by trophic group and by fish family. Sample events data contains \emph{mean} total biomass in kg/ha per sample event, and by trophic group and by fish family, as well as the  \emph{standard deviation} of the total biomass as well as per trophic group and fish family.
#'
#' Benthic LIT method data is available by setting \code{method} to "benthiclit". Benthic LIT observations contain individual observations. Sample units data returns percent cover per sample unit, by benthic category. Sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.
#'
#' Benthic PIT method data is available by setting \code{method} to "benthicpit". Similarly to Benthic LIT, Benthic PIT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category, and sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.
#'
#' Benthic Photo Quadrat method data is available by setting \code{method} to "benthicpqt". Benthic PQT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category, and sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.
#'
#' Bleaching method data is available by setting \code{method} to "bleaching". When Bleaching observations are requested, two types of observations are returned: Colonies Bleached and Percent Cover. Sample units data contains both Colonies Bleached data (number of coral genera and total number of colonies, and percent normal, pale, and bleached colonies) and Percent Cover data (Number of quadrats, and average percent cover for hard coral, soft coral, and macroalgae), all per sample unit. Sample events data contains \emph{mean} values of all the data in sample units, for both Colonies Bleached (average quadrat size, average number of coral genera and average total colonies, average percent normal, pale, and bleached colonies) and Percent Cover (average number of quadrats, and average of average hard coral, soft coral, and macroalgae cover). Mean percent cover values also come with standard deviations.
#'
#' Habitat Complexity data is available by setting \code{method} to "habitatcomplexity". Observations contain individual observations, with the habitat complexity score at each interval. Sample units data contains the average habitat complexity score for the sample unit, and sample events data contains the average of those averages habitat complexity scores, along with standard deviations.
#'
#' The included covariates are: geomorphic zonation and benthic habitat from the \href{https://allencoralatlas.org}{Allen Coral Atlas}; market gravity, water pollution (sediments and nitrogen), coastal population, industrial development (number of ports), tourism (reef value), and a cumulative pressure index from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12858}{\emph{A global map of human pressures on tropical coral reefs}} by Andrello et al., 2022; scores from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12587}{\emph{Risk-sensitive planning for conserving coral reefs under rapid climate change}} by Beyer et al., 2018.
#'
#' @param method Method to get data for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", bleaching", "habitatcomplexity", or "all" (to get data for all methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or "all" (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @inheritParams mermaid_get_project_sites
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_data(method = "fishbelt", data = "observations", limit = 10)
#'
#' projects %>%
#'   mermaid_get_project_data(method = c("benthicpit", "fishbelt"), data = "sampleevents", limit = 10)
#'
#' bleaching_obs <- projects %>%
#'   mermaid_get_project_data(method = "bleaching", data = "observations", limit = 10)
#' names(bleaching_obs)
#' # [1] "colonies_bleached" "percent_cover"
#' }
mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, token = mermaid_token(), covariates = FALSE) {
  internal_mermaid_get_project_data(project, method, data, limit, covariates = covariates, legacy = FALSE, token)
}

internal_mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, covariates = FALSE, legacy = legacy, token = mermaid_token()) {
  check_project_data_inputs(method, data)

  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity")
  }
  if (any(data == "all")) {
    data <- c("observations", "sampleunits", "sampleevents")
  }

  endpoint <- construct_endpoint(method, data, legacy)

  res <- purrr::map(endpoint, function(x) get_project_endpoint(project, x, limit, token, covariates = covariates))

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

  # If covariates = TRUE, get sites for each project
  if (covariates) {
    project_sites_covariates <- project %>%
      mermaid_get_project_sites(covariates = TRUE) %>%
      dplyr::select(
        tidyselect::any_of("project"),
        tidyselect::all_of(c(
          site_id = "id",
          covars_cols
        ))
      )

    # Join covariates to any DFs in res
    res <- res %>%
      add_covariates_to_data(project_sites_covariates)
  }

  if (length(endpoint) == 1) {
    res[[1]]
  } else {
    names(res) <- method
    res
  }
}

mermaid_get_project_data_legacy <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, token = mermaid_token(), covariates = FALSE) {
  internal_mermaid_get_project_data(project, method, data, limit, covariates = covariates, legacy = TRUE, token)
}

check_project_data_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching", "all"))) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"', call. = FALSE)
  }
  if (!all(data %in% c("observations", "sampleunits", "sampleevents", "all"))) {
    stop('`data` must be one of: "observations", "sampleunits", "sampleevents", "all"', call. = FALSE)
  }
}

construct_endpoint <- function(method, data, legacy) {
  method_data <- tidyr::expand_grid(method = method, data = data)

  method_data <- method_data %>%
    dplyr::mutate(
      method = dplyr::case_when(
        .data$method == "fishbelt" ~ "beltfishes",
        .data$method == "benthicpit" ~ "benthicpits",
        .data$method == "benthiclit" ~ "benthiclits",
        .data$method == "benthicpqt" ~ "benthicpqts",
        .data$method == "habitatcomplexity" ~ "habitatcomplexities",
        .data$method == "bleaching" ~ "bleachingqcs"
      ),
      data = dplyr::case_when(
        .data$data == "observations" & .data$method == "beltfishes" ~ "obstransectbeltfishes",
        .data$data == "observations" & .data$method == "benthicpits" ~ "obstransectbenthicpits",
        .data$data == "observations" & .data$method == "benthicpqts" ~ "obstransectbenthicpqts",
        .data$data == "observations" & .data$method == "benthiclits" ~ "obstransectbenthiclits",
        .data$data == "observations" & .data$method == "habitatcomplexities" ~ "obshabitatcomplexities",
        .data$data == "observations" & .data$method == "bleachingqcs" ~ "obscoloniesbleacheds,obsquadratbenthicpercents",
        TRUE ~ data
      )
    ) %>%
    tidyr::separate_rows(method, data, sep = ",")

  csv_endpoint <- ifelse(legacy, "", "/csv")

  method_data <- method_data %>%
    dplyr::mutate(endpoint = paste0(.data$method, "/", .data$data, csv_endpoint))

  method_data_list <- method_data %>%
    split(method_data$method) %>%
    purrr::map(dplyr::pull, .data$endpoint)

  method_data_list[unique(method_data[["method"]])]
}

covars_cols <- c("aca_geomorphic", "aca_benthic", "andrello_grav_nc", "andrello_sediment", "andrello_nutrient", "andrello_pop_count", "andrello_num_ports", "andrello_reef_value", "andrello_cumul_score", "beyer_score", "beyer_scorecn", "beyer_scorecy", "beyer_scorepfc", "beyer_scoreth", "beyer_scoretr")

common_cols <- list(
  "obs/su" = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "relative_depth", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth"),
  se = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "depth_sd"),
  obs_closing = c("project_notes", "site_notes", "management_notes", "sample_unit_id", "sample_event_id", "project_admins", "contact_link"),
  su_closing = c("project_notes", "site_notes", "management_notes", "sample_unit_notes", "sample_event_id", "sample_unit_ids", "project_admins", "contact_link"),
  se_closing = c("observers", "project_notes", "site_notes", "management_notes", "id", "sample_unit_count", "project_admins", "contact_link", "sample_event_id"),
  life_histories_obs = "life_histories",
  life_histories_obs_csv = c("life_histories__competitive", "life_histories__generalist", "life_histories__stress-tolerant", "life_histories__weedy"),
  life_histories_su = "percent_cover_life_histories",
  life_histories_su_csv = c("percent_cover_life_histories_weedy", "percent_cover_life_histories_generalist", "percent_cover_life_histories_competitive", "percent_cover_life_histories_stress-tolerant"),
  life_histories_se = c("percent_cover_life_histories_avg", "percent_cover_life_histories_sd"),
  life_histories_se_csv = c("percent_cover_life_histories_avg_weedy", "percent_cover_life_histories_avg_generalist", "percent_cover_life_histories_avg_competitive", "percent_cover_life_histories_avg_stress-tolerant", "percent_cover_life_histories_sd_weedy", "percent_cover_life_histories_sd_generalist", "percent_cover_life_histories_sd_competitive", "percent_cover_life_histories_sd_stress-tolerant")
)

common_benthic_cols <- list(
  obs = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "benthic_category", "benthic_attribute", "growth_form"),
  su = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "percent_cover_benthic_category"),
  se = c(common_cols[["se"]], "percent_cover_benthic_category_avg", "percent_cover_benthic_category_sd")
)

common_bleaching_cols <- list(
  obs = c(common_cols[["obs/su"]][!common_cols[["obs/su"]] == "reef_slope"], "quadrat_size", "label", "observers"),
  su = c(common_cols[["obs/su"]][!common_cols[["obs/su"]] == "reef_slope"], "quadrat_size", "label", "count_total", "count_genera", "percent_normal", "percent_pale", "percent_bleached", "quadrat_count", "percent_hard_avg", "percent_hard_sd", "percent_soft_avg", "percent_soft_sd", "percent_algae_avg", "percent_algae_sd"),
  se = c(common_cols[["se"]], "quadrat_size_avg", "count_total_avg", "count_total_sd", "count_genera_avg", "count_genera_sd", "percent_normal_avg", "percent_normal_sd", "percent_pale_avg", "percent_pale_sd", "percent_bleached_avg", "percent_bleached_sd", "quadrat_count_avg", "percent_hard_avg_avg", "percent_hard_avg_sd", "percent_soft_avg_avg", "percent_soft_avg_sd", "percent_algae_avg_avg", "percent_algae_avg_sd")
)

# For select columns and setting order
project_data_columns <- list(

  # Fishbelt
  `beltfishes/obstransectbeltfishes` = c(common_cols[["obs/su"]], "transect_length", "transect_width", "assigned_transect_width_m", "size_bin", "observers", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "count", "biomass_kgha", "trophic_level", "trophic_group", "functional_group", "vulnerability", "data_policy_beltfish", common_cols[["obs_closing"]]),
  `beltfishes/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "label", "size_bin", "transect_length", "transect_width", "biomass_kgha", "total_abundance", "biomass_kgha_trophic_group", "biomass_kgha_fish_family", "data_policy_beltfish", common_cols[["su_closing"]]),
  `beltfishes/sampleevents` = c(common_cols[["se"]], "biomass_kgha_avg", "biomass_kgha_sd", "biomass_kgha_trophic_group_avg", "biomass_kgha_trophic_group_sd", "biomass_kgha_fish_family_avg", "biomass_kgha_fish_family_sd", "data_policy_beltfish", common_cols[["se_closing"]]),

  # Benthic PIT

  `benthicpits/obstransectbenthicpits` = c(
    common_benthic_cols[["obs"]],
    "interval", "interval_start", "interval_size",
    common_cols[["life_histories_obs"]],
    "data_policy_benthicpit",
    common_cols[["obs_closing"]]
  ),
  `benthicpits/obstransectbenthicpits/csv` = c(
    common_benthic_cols[["obs"]],
    "interval", "interval_start", "interval_size",
    common_cols[["life_histories_obs_csv"]],
    "data_policy_benthicpit",
    common_cols[["obs_closing"]]
  ),
  `benthicpits/sampleunits` = c(
    common_benthic_cols[["su"]],
    "interval_start", "interval_size",
    common_cols[["life_histories_su"]],
    "data_policy_benthicpit",
    common_cols[["su_closing"]]
  ),
  `benthicpits/sampleunits/csv` = c(
    common_benthic_cols[["su"]],
    "interval_start", "interval_size",
    common_cols[["life_histories_su_csv"]],
    "data_policy_benthicpit",
    common_cols[["su_closing"]]
  ),
  `benthicpits/sampleevents` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se"]],
    "data_policy_benthicpit",
    common_cols[["se_closing"]]
  ),
  `benthicpits/sampleevents/csv` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se_csv"]],
    "data_policy_benthicpit",
    common_cols[["se_closing"]]
  ),

  # Benthic LIT

  `benthiclits/obstransectbenthiclits` = c(
    common_benthic_cols[["obs"]],
    "length", "total_length",
    common_cols[["life_histories_obs"]],
    "data_policy_benthiclit",
    common_cols[["obs_closing"]]
  ),
  `benthiclits/obstransectbenthiclits/csv` = c(
    common_benthic_cols[["obs"]],
    "length", "total_length",
    common_cols[["life_histories_obs_csv"]],
    "data_policy_benthiclit",
    common_cols[["obs_closing"]]
  ),
  `benthiclits/sampleunits` = c(
    common_benthic_cols[["su"]],
    "total_length",
    common_cols[["life_histories_su"]],
    "data_policy_benthiclit",
    common_cols[["su_closing"]]
  ),
  `benthiclits/sampleunits/csv` = c(
    common_benthic_cols[["su"]],
    "total_length",
    common_cols[["life_histories_su_csv"]],
    "data_policy_benthiclit",
    common_cols[["su_closing"]]
  ),
  `benthiclits/sampleevents` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se"]],
    "data_policy_benthiclit",
    common_cols[["se_closing"]]
  ),
  `benthiclits/sampleevents/csv` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se_csv"]],
    "data_policy_benthiclit",
    common_cols[["se_closing"]]
  ),

  # Benthic PQT

  `benthicpqts/obstransectbenthicpqts` = c(
    common_benthic_cols[["obs"]],
    common_cols[["life_histories_obs"]],
    "data_policy_benthicpqt",
    common_cols[["obs_closing"]]
  ),
  `benthicpqts/obstransectbenthicpqts/csv` = c(
    common_benthic_cols[["obs"]],
    common_cols[["life_histories_obs_csv"]],
    "data_policy_benthicpqt",
    common_cols[["obs_closing"]]
  ),
  `benthicpqts/sampleunits` = c(
    common_benthic_cols[["su"]],
    common_cols[["life_histories_su"]],
    "data_policy_benthicpqt",
    common_cols[["su_closing"]]
  ),
  `benthicpqts/sampleunits/csv` = c(
    common_benthic_cols[["su"]],
    common_cols[["life_histories_su_csv"]],
    "data_policy_benthicpqt",
    common_cols[["su_closing"]]
  ),
  `benthicpqts/sampleevents` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se"]],
    "data_policy_benthicpqt",
    common_cols[["se_closing"]]
  ),
  `benthicpqts/sampleevents/csv` = c(
    common_benthic_cols[["se"]],
    common_cols[["life_histories_se_csv"]],
    "data_policy_benthicpqt",
    common_cols[["se_closing"]]
  ),

  # Habitat complexity

  `habitatcomplexities/obshabitatcomplexities` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "interval_size", "observers", "interval", "score", "score_name", "data_policy_habitatcomplexity", common_cols[["obs_closing"]]),
  `habitatcomplexities/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "score_avg", "score_sd", "data_policy_habitatcomplexity", common_cols[["su_closing"]]),
  `habitatcomplexities/sampleevents` = c(common_cols[["se"]], "score_avg_avg", "score_avg_sd", "data_policy_habitatcomplexity", common_cols[["se_closing"]]),

  # Bleaching

  `bleachingqcs/obscoloniesbleacheds` =
    c(
      common_bleaching_cols[["obs"]],
      "benthic_attribute",
      "benthic_category",
      "growth_form",
      "count_normal",
      "count_pale",
      "count_20",
      "count_50",
      "count_80",
      "count_100",
      "count_dead",
      common_cols[["life_histories_obs"]],
      "data_policy_bleachingqc",
      common_cols[["obs_closing"]]
    ),
  `bleachingqcs/obscoloniesbleached/csv` =
    c(
      common_bleaching_cols[["obs"]],
      "benthic_attribute",
      "benthic_category",
      "growth_form",
      "count_normal",
      "count_pale",
      "count_20",
      "count_50",
      "count_80",
      "count_100",
      "count_dead",
      common_cols[["life_histories_obs_csv"]],
      "data_policy_bleachingqc",
      common_cols[["obs_closing"]]
    ),
  `bleachingqcs/obsquadratbenthicpercents` =
    c(
      common_bleaching_cols[["obs"]],
      "quadrat_number",
      "percent_hard",
      "percent_soft",
      "percent_algae",
      "data_policy_bleachingqc",
      common_cols[["obs_closing"]]
    ),
  `bleachingqcs/obsquadratbenthicpercents/csv` =
    c(
      common_bleaching_cols[["obs"]],
      "quadrat_number",
      "percent_hard",
      "percent_soft",
      "percent_algae",
      "data_policy_bleachingqc",
      common_cols[["obs_closing"]]
    ),
  `bleachingqcs/sampleunits` = c(
    common_bleaching_cols[["su"]],
    common_cols[["life_histories_su"]],
    "data_policy_bleachingqc",
    common_cols[["su_closing"]]
  ),
  `bleachingqcs/sampleunits/csv` = c(
    common_bleaching_cols[["su"]],
    common_cols[["life_histories_su_csv"]],
    "data_policy_bleachingqc",
    common_cols[["su_closing"]]
  ),
  `bleachingqcs/sampleevents` = c(
    common_bleaching_cols[["se"]],
    "data_policy_bleachingqc",
    common_cols[["life_histories_se"]],
    common_cols[["se_closing"]]
  ),
  `bleachingqcs/sampleevents/csv` = c(
    common_bleaching_cols[["se"]],
    "data_policy_bleachingqc",
    common_cols[["life_histories_se_csv"]],
    common_cols[["se_closing"]]
  )
)

project_data_columns_csv <- project_data_columns[!stringr::str_ends(names(project_data_columns), "/csv")]
names(project_data_columns_csv) <- paste0(names(project_data_columns_csv), "/csv")

project_data_columns_csv <- append(project_data_columns_csv[!names(project_data_columns_csv) %in% names(project_data_columns)], project_data_columns[stringr::str_ends(names(project_data_columns), "csv")])

project_data_columns_csv <- project_data_columns_csv %>%
  purrr::map(~ c(.x, "sample_date_year", "sample_date_month", "sample_date_day"))

project_data_columns <- append(project_data_columns[!names(project_data_columns) %in% names(project_data_columns_csv)], project_data_columns_csv)

# For testing columns, after df-cols have been expanded
testing_cols <- list(
  benthic_obs = "life_histories",
  benthic_su = c("percent_cover_benthic_category", "percent_cover_life_histories"),
  benthic_se = c("percent_cover_benthic_category_avg", "percent_cover_benthic_category_sd", "percent_cover_life_histories_avg", "percent_cover_life_histories_sd"),
  bleaching_obs = "life_histories",
  bleaching_su = c("percent_cover_life_histories"),
  bleaching_se = c("percent_cover_life_histories_avg", "percent_cover_life_histories_sd")
)

project_data_df_columns_list <- list(
  `beltfishes/sampleunits` = c("biomass_kgha_trophic_group", "biomass_kgha_fish_family"),
  `beltfishes/sampleevents` = c("biomass_kgha_trophic_group_avg", "biomass_kgha_fish_family_avg", "biomass_kgha_trophic_group_sd", "biomass_kgha_fish_family_sd"),
  `benthicpits/obstransectbenthicpits` = testing_cols[["benthic_obs"]],
  `benthiclits/obstransectbenthiclits` = testing_cols[["benthic_obs"]],
  `benthicpqts/obstransectbenthicpqts` = testing_cols[["benthic_obs"]],
  `bleachingqcs/obscoloniesbleacheds` = testing_cols[["bleaching_obs"]],
  `benthicpits/sampleunits` = testing_cols[["benthic_su"]],
  `benthicpits/sampleevents` = testing_cols[["benthic_se"]],
  `benthiclits/sampleunits` = testing_cols[["benthic_su"]],
  `benthiclits/sampleevents` = testing_cols[["benthic_se"]],
  `benthicpqts/sampleunits` = testing_cols[["benthic_su"]],
  `benthicpqts/sampleevents` = testing_cols[["benthic_se"]],
  `bleachingqcs/sampleunits` = testing_cols[["bleaching_su"]],
  `bleachingqcs/sampleevents` = testing_cols[["bleaching_se"]]
)

project_data_df_columns_list_csv <- project_data_df_columns_list
names(project_data_df_columns_list_csv) <- paste0(names(project_data_df_columns_list_csv), "/csv")

project_data_df_columns_list <- append(project_data_df_columns_list, project_data_df_columns_list_csv)

project_data_df_columns_list_names <- project_data_df_columns_list %>%
  purrr::map(paste0, collapse = "|")

project_data_df_columns <- project_data_df_columns_list %>%
  purrr::map_df(dplyr::as_tibble, .id = "endpoint")

project_data_test_columns <- project_data_columns %>%
  purrr::map_df(dplyr::as_tibble, .id = "endpoint") %>%
  dplyr::distinct() %>%
  dplyr::anti_join(project_data_df_columns, by = c("endpoint", "value")) %>%
  dplyr::filter(!value %in% c("sample_date_year", "sample_date_month", "sample_date_day")) %>%
  split(.$endpoint) %>%
  purrr::map(dplyr::pull, value) %>%
  purrr::map(snakecase::to_snake_case)

add_covariates_to_data <- function(data, covariates) {
  for (i in names(data)) {
    if (inherits(data[[i]], "tbl_df")) {
      if (nrow(data[[i]]) == 0) {
        covars_temp <- covariates %>%
          dplyr::select(tidyselect::all_of(covars_cols)) %>%
          dplyr::slice(0)
        data[[i]] <- data[[i]] %>%
          dplyr::bind_cols(covars_temp)
      } else {
        if ("project" %in% names(covariates)) {
          data[[i]] <- data[[i]] %>%
            dplyr::left_join(covariates, by = c("project", "site_id"))
        } else {
          data[[i]] <- data[[i]] %>%
            dplyr::left_join(covariates, by = "site_id")
        }
      }

      # Move to before "managements"
      data[[i]] <- data[[i]] %>%
        dplyr::relocate(tidyselect::all_of(covars_cols), .before = "management")
    } else {
      if (inherits(data[[i]], "list")) {
        data[[i]] <- data[[i]] %>%
          add_covariates_to_data(covariates)
      }
    }
  }

  data
}
