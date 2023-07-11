internal_mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, legacy = FALSE, covariates = FALSE, token = mermaid_token()) {
  check_project_data_inputs(method, data)

  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity")
  }
  if (any(data == "all")) {
    data <- c("observations", "sampleunits", "sampleevents")
  }

  endpoint <- construct_endpoint(method, data, legacy)

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit, token))

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
      dplyr::select(tidyselect::any_of("project"),
        site = .data$name,
        tidyselect::all_of(covars_cols)
      )

    # Join covariates to any DFs in ress
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
  "se" = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg"),
  "obs_closing" = c("project_notes", "site_notes", "management_notes", "sample_unit_id", "sample_event_id", "contact_link"),
  "su_closing" = c("project_notes", "site_notes", "management_notes", "sample_unit_notes", "sample_event_id", "sample_unit_ids", "id", "contact_link"),
  "se_closing" = c("project_notes", "site_notes", "management_notes", "id", "sample_unit_count", "contact_link")
)

# For select columns and setting order
project_data_columns <- list(
  `benthicpqts/obstransectbenthicpqts` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "quadrat_size", "num_quadrats", "num_points_per_quadrat", "observers", "quadrat_number", "benthic_category", "benthic_attribute", "growth_form", "num_points", "data_policy_benthicpqt", common_cols[["obs_closing"]]),
  `benthicpqts/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpqt", common_cols[["su_closing"]]),
  `benthicpqts/sampleevents` = c(common_cols[["se"]], "percent_cover_by_benthic_category_avg", "data_policy_benthicpqt", common_cols[["se_closing"]]),
  `beltfishes/obstransectbeltfishes` = c(common_cols[["obs/su"]], "transect_length", "transect_width", "size_bin", "observers", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "count", "biomass_kgha", "trophic_level", "trophic_group", "functional_group", "vulnerability", "data_policy_beltfish", common_cols[["obs_closing"]]),
  `beltfishes/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "label", "size_bin", "transect_length", "transect_width", "biomass_kgha", "total_abundance", "biomass_kgha_by_trophic_group", "biomass_kgha_by_fish_family", "data_policy_beltfish", common_cols[["su_closing"]]),
  `beltfishes/sampleevents` = c(common_cols[["se"]], "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "biomass_kgha_by_fish_family_avg", "data_policy_beltfish", common_cols[["se_closing"]]),
  `benthicpits/obstransectbenthicpits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", common_cols[["obs_closing"]]),
  `benthicpits/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "interval_start", "interval_size", "observers", "percent_cover_by_benthic_category", "data_policy_benthicpit", common_cols[["su_closing"]]),
  `benthicpits/sampleevents` = c(common_cols[["se"]], "percent_cover_by_benthic_category_avg", "data_policy_benthicpit", common_cols[["se_closing"]]),
  `benthiclits/obstransectbenthiclits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "benthic_category", "benthic_attribute", "growth_form", "length", "total_length", "data_policy_benthiclit", common_cols[["obs_closing"]]),
  `benthiclits/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "total_length", "percent_cover_by_benthic_category", "data_policy_benthiclit", common_cols[["su_closing"]]),
  `benthiclits/sampleevents` = c(common_cols[["se"]], "percent_cover_by_benthic_category_avg", "data_policy_benthiclit", common_cols[["se_closing"]]),
  `habitatcomplexities/obshabitatcomplexities` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "interval_size", "observers", "interval", "score", "score_name", "data_policy_habitatcomplexity", common_cols[["obs_closing"]]),
  `habitatcomplexities/sampleunits` = c(common_cols[["obs/su"]], "transect_number", "transect_length", "label", "observers", "score_avg", "data_policy_habitatcomplexity", common_cols[["su_closing"]]),
  `habitatcomplexities/sampleevents` = c(common_cols[["se"]], "depth_avg", "score_avg_avg", "data_policy_habitatcomplexity", common_cols[["se_closing"]]),
  `bleachingqcs/obscoloniesbleacheds` = c(common_cols[["obs/su"]][!common_cols[["obs/su"]] == "reef_slope"], "quadrat_size", "label", "observers", "benthic_attribute", "growth_form", "count_normal", "count_pale", "count_20", "count_50", "count_80", "count_100", "count_dead", "data_policy_bleachingqc", common_cols[["obs_closing"]]),
  `bleachingqcs/obsquadratbenthicpercents` = c(common_cols[["obs/su"]][!common_cols[["obs/su"]] == "reef_slope"], "quadrat_size", "label", "observers", "quadrat_number", "percent_hard", "percent_soft", "percent_algae", "data_policy_bleachingqc", common_cols[["obs_closing"]]),
  `bleachingqcs/sampleunits` = c(common_cols[["obs/su"]][!common_cols[["obs/su"]] == "reef_slope"], "quadrat_size", "label", "count_total", "count_genera", "percent_normal", "percent_pale", "percent_bleached", "quadrat_count", "percent_hard_avg", "percent_soft_avg", "percent_algae_avg", "data_policy_bleachingqc", common_cols[["su_closing"]]),
  `bleachingqcs/sampleevents` = c(common_cols[["se"]], "quadrat_size_avg", "count_total_avg", "count_genera_avg", "percent_normal_avg", "percent_pale_avg", "percent_bleached_avg", "quadrat_count_avg", "percent_hard_avg_avg", "percent_soft_avg_avg", "percent_algae_avg_avg", "data_policy_bleachingqc", common_cols[["se_closing"]])
)

project_data_columns_csv <- project_data_columns
names(project_data_columns_csv) <- paste0(names(project_data_columns), "/csv")

project_data_columns_csv <- project_data_columns_csv %>%
  purrr::map(~ c(.x, "sample_date_year", "sample_date_month", "sample_date_day"))

project_data_columns <- append(project_data_columns, project_data_columns_csv)

# For testing columns, after df-cols have been expanded
project_data_df_columns_list <- list(
  `beltfishes/sampleunits` = c("biomass_kgha_by_trophic_group", "biomass_kgha_by_fish_family"),
  `beltfishes/sampleevents` = c("biomass_kgha_by_trophic_group_avg", "biomass_kgha_by_fish_family_avg"),
  `benthicpits/sampleunits` = c("percent_cover_by_benthic_category"),
  `benthicpits/sampleevents` = c("percent_cover_by_benthic_category_avg"),
  `benthiclits/sampleunits` = c("percent_cover_by_benthic_category"),
  `benthiclits/sampleevents` = c("percent_cover_by_benthic_category_avg"),
  `benthicpqts/sampleunits` = c("percent_cover_by_benthic_category"),
  `benthicpqts/sampleevents` = c("percent_cover_by_benthic_category_avg")
)

project_data_df_columns_list_names <- project_data_df_columns_list %>%
  purrr::map(stringr::str_remove_all, "_by") %>%
  purrr::map(paste0, collapse = "|")

project_data_df_columns <- project_data_df_columns_list %>%
  purrr::map_df(dplyr::as_tibble, .id = "endpoint")

project_data_test_columns <- project_data_columns %>%
  purrr::map_df(dplyr::as_tibble, .id = "endpoint") %>%
  dplyr::anti_join(project_data_df_columns, by = c("endpoint", "value")) %>%
  split(.$endpoint) %>%
  purrr::map(dplyr::pull, value)


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
          # Always coerce site to character in both, in case of sites with all numeric names causing join issues
          data[[i]] <- data[[i]] %>%
            dplyr::mutate(site = as.character(.data$site)) %>%
            dplyr::left_join(covariates %>%
              dplyr::mutate(site = as.character(.data$site)), by = c("project", "site"))
        } else {
          data[[i]] <- data[[i]] %>%
            dplyr::mutate(site = as.character(.data$site)) %>%
            dplyr::left_join(covariates %>%
              dplyr::mutate(site = as.character(.data$site)), by = "site")
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
