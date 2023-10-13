test_that("mermaid_get_project_data returns a data frame with the correct names", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects()
  output <- mermaid_get_project_data(p, method = "benthicpit", data = "sampleunits", limit = 1)
  expect_true(all(project_data_test_columns[["benthicpits/sampleunits"]] %in% names(output)))
  expect_true(any(stringr::str_starts(names(output), project_data_df_columns_list_names[["benthicpits/sampleunits"]])))
  expect_true(nrow(output) >= 1)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_project_data allows multiple methods", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"
  output <- mermaid_get_project_data(p, method = c("fishbelt", "benthicpit", "benthiclit"), data = "sampleunits", limit = 1)
  expect_named(output, c("fishbelt", "benthicpit", "benthiclit"))
})

test_that("mermaid_get_project_data allows multiple forms of data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"
  output <- mermaid_get_project_data(p, method = "fishbelt", data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data allows multiple methods and multiple forms of data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- c("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a")
  output <- mermaid_get_project_data(p, method = c("fishbelt", "benthicpit"), data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_named(output, c("fishbelt", "benthicpit"))
  expect_named(output[["fishbelt"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpit"]], c("observations", "sampleunits", "sampleevents"))
  expect_true(all(project_data_test_columns[["benthicpits/sampleunits"]] %in% names(output[["benthicpit"]][["sampleunits"]])))
  expect_true(any(stringr::str_starts(names(output[["benthicpit"]][["sampleunits"]]), project_data_df_columns_list_names[["benthicpits/sampleunits"]])))
  expect_named(output[["fishbelt"]][["observations"]], project_data_test_columns[["beltfishes/obstransectbeltfishes"]])
})

test_that("mermaid_get_project_data errors if passed a wrong method or data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"
  expect_error(mermaid_get_project_data(p, method = "beltfishs", data = "sampleunits"), "one of")
  expect_error(mermaid_get_project_data(p, method = "benthicpits", data = "samplevents"), "one of")
})

test_that("mermaid_get_project_data setting 'all' works", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"
  output <- mermaid_get_project_data(p, method = "all", data = "all", limit = 1)
  expect_named(output, c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"))
  purrr::walk(output, expect_named, c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data with 'bleaching' method and 'observations' data returns a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "observations", limit = 1)
  expect_named(output, c("colonies_bleached", "percent_cover"))
  expect_named(output[["colonies_bleached"]], project_data_test_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["percent_cover"]], project_data_test_columns[["bleachingqcs/obsquadratbenthicpercents"]])
})

test_that("mermaid_get_project_data with 'bleaching' method and multiple values for `data` (including 'observations') returns the 'observations' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "all", limit = 1)
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["observations"]][["colonies_bleached"]], project_data_test_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["observations"]][["percent_cover"]], project_data_test_columns[["bleachingqcs/obsquadratbenthicpercents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "observations", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "observations", "sampleunits"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["observations"]][["colonies_bleached"]], project_data_test_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["observations"]][["percent_cover"]], project_data_test_columns[["bleachingqcs/obsquadratbenthicpercents"]])
})

test_that("mermaid_get_project_data with multiple `methods` (including 'bleaching') returns the 'bleaching' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("fishbelt", "bleaching"), "observations", limit = 1)
  expect_named(output, c("fishbelt", "bleaching"))
  expect_named(output[["bleaching"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["bleaching"]][["colonies_bleached"]], project_data_test_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["bleaching"]][["percent_cover"]], project_data_test_columns[["bleachingqcs/obsquadratbenthicpercents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("bleaching", "benthiclit"), "all", limit = 1)
  expect_named(output, c("bleaching", "benthiclit"))
  expect_named(output[["bleaching"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["bleaching"]][["observations"]], c("colonies_bleached", "percent_cover"))
})

test_that("mermaid_get_project_data with multiple data returns a list with multiple elements in the same order that they were supplied", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleunits", "sampleevents"), limit = 1)
  expect_named(output, c("sampleunits", "sampleevents"))
  expect_named(output[["sampleunits"]], project_data_test_columns[["bleachingqcs/sampleunits"]])
  expect_named(output[["sampleevents"]], project_data_test_columns[["bleachingqcs/sampleevents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "sampleunits"))
  expect_named(output[["sampleunits"]], project_data_test_columns[["bleachingqcs/sampleunits"]])
  expect_named(output[["sampleevents"]], project_data_test_columns[["bleachingqcs/sampleevents"]])
})

test_that("mermaid_get_project_data with multiple methods returns a list with multiple elements in the same order that they were supplied", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("bleaching", "benthicpit"), "sampleevents", limit = 1)
  expect_named(output, c("bleaching", "benthicpit"))
  expect_named(output[["bleaching"]], project_data_test_columns[["bleachingqcs/sampleevents"]])
  expect_true(all(project_data_test_columns[["benthicpits/sampleevents"]] %in% names(output[["benthicpit"]])))

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("benthicpit", "bleaching"), "sampleevents", limit = 1)
  expect_named(output, c("benthicpit", "bleaching"))
  expect_named(output[["bleaching"]], project_data_test_columns[["bleachingqcs/sampleevents"]])
  expect_true(all(project_data_test_columns[["benthicpits/sampleevents"]] %in% names(output[["benthicpit"]])))
})

test_that("mermaid_get_project_data does not return the df-column in cases where there is no data: not for a single project and one endpoint, nor for a single project and multiple endpoints, nor for multiple projects (one of which has data, one of which does not), nor for multiple projects (neither of which have data)", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_named(mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "benthicpit", "sampleevents"), project_data_test_columns[["benthicpits/sampleevents"]])
  expect_named(mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "benthicpit", "sampleunits"), project_data_test_columns[["benthicpits/sampleunits"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "benthicpit", c("sampleunits", "sampleevents"))
  expect_named(output[["sampleunits"]], project_data_test_columns[["benthicpits/sampleunits"]])
  expect_named(output[["sampleevents"]], project_data_test_columns[["benthicpits/sampleevents"]])

  # One project with, one without
  output <- mermaid_get_project_data(c("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"), "benthicpit", "sampleunits")
  expect_false("percent_cover_benthic_category" %in% names(output))

  # Multiple without
  output <- mermaid_get_project_data(c("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "4d23d2a1-774f-4ccf-b567-69f95e4ff572"), "benthicpit", "sampleunits")
  expect_named(output, project_data_test_columns[["benthicpits/sampleunits"]])
  expect_false("percent_cover_benthic_category" %in% names(output))
})


# Testing aggregation views ----

# Fishbelt ----

## Vanilla fishbelt ----

test_that("Vanilla fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations") %>%
    construct_fake_sample_unit_id()

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  sus_minus_zeros <- sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus_minus_zeros)

  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family

  obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)

  # Check that values match

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Vanilla fishbelt sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "fishbelt", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate sample units to sample events - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc - but will want to check these in the other fishbelts!
  # Just aggregate straight up to calculate depth_avg, biomass_kgha_avg, biomass_kgha_trophic_group_avg, and biomass_kgha_fish_family_avg

  sus_agg_for_se_comparison <- calculate_sus_biomass_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_biomass_avg_long(ses)

  # Check that values match
  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

## Variable widths ----

test_that("Variables widths fishbelt observations view biomass is the same as manually calculating biomass", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")

  # Biomass is calculated as:
  # 10 * count * biomass_constant_a * (size * biomass_constant_c) ^ biomass_constant_b / (transect_length * width)
  # In the mixed width case, the width depends on the size
  # In this project, the width is: 2m if size < 10cm, 5m if size >= 10cm

  obs_biomass_calc <- obs %>%
    dplyr::mutate(
      width = dplyr::case_when(
        size < 10 ~ 2,
        size >= 10 ~ 5
      ),
      biomass_kgha_calc = 10 * count * biomass_constant_a * (size * biomass_constant_c)^biomass_constant_b / (transect_length * width),
      biomass_kgha_calc = round(biomass_kgha_calc, 2),
      match = biomass_kgha == biomass_kgha_calc
    )

  expect_true(all(obs_biomass_calc[["match"]]))
})

test_that("Variable widths fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  sus_minus_zeros <- sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus_minus_zeros)

  # Aggregate observations to sample units - there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family

  obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)

  # Check that values match

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Variable widths fishbelt sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "fishbelt", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate sample units to sample events - calculate depth_avg, biomass_kgha_avg, biomass_kgha_trophic_group_avg, and biomass_kgha_fish_family_avg, and compare to SE values

  sus_agg_for_se_comparison <- calculate_sus_biomass_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_biomass_avg_long(ses)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

## Big/small fish ----

test_that("Big/small fish fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "507d1af9-edbd-417e-a65c-350f8bba1299"

  # TODO
  # obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")
  #
  # sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")
  #
  # obs <- obs %>%
  #   construct_fake_sample_unit_id()
  #
  # # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons
  #
  # sus_minus_zeros <- sus %>%
  #   dplyr::filter(biomass_kgha != 0) %>%
  #   construct_fake_sample_unit_id()
  #
  # # Check first that there are the same number of fake SUs as real SUs
  # test_n_fake_sus(obs, sus_minus_zeros)
  #
  # # Check that su.sample_unit_ids contains obs.sample_unit_id for cases where they have the same fake_sample_unit_id
  #
  # sus_ids <- sus_minus_zeros %>%
  #   dplyr::select(fake_sample_unit_id, sample_unit_id = sample_unit_ids) %>%
  #   tidyr::separate_rows(sample_unit_id, sep = "; ") %>%
  #   dplyr::arrange(fake_sample_unit_id, sample_unit_id)
  #
  # obs_ids <- obs %>%
  #   dplyr::select(fake_sample_unit_id, sample_unit_id) %>%
  #   dplyr::distinct() %>%
  #   dplyr::arrange(fake_sample_unit_id, sample_unit_id)
  #
  # expect_identical(sus_ids, obs_ids)
  #
  # # Check that every sample unit has a big/small transect
  # # This means that each "fake" sample unit id has 2 (pseudo) sample unit ids
  # expect_equal(sus_ids %>%
  #   dplyr::count(fake_sample_unit_id) %>%
  #   dplyr::pull(n) %>%
  #   unique(), 2)
  #
  # # Also means that every set of observations is either BF or SF, and has a corresponding SF/BF
  # expect_identical(
  #   obs %>%
  #     dplyr::distinct(fake_sample_unit_id, label) %>%
  #     dplyr::group_by(fake_sample_unit_id) %>%
  #     dplyr::summarise(
  #       label = paste0(sort(label), collapse = ","),
  #       .groups = "drop"
  #     ) %>%
  #     dplyr::pull(label) %>%
  #     unique(),
  #   "BF,SF"
  # )
  #
  # # Aggregate observations to sample units
  # # Calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family
  # # Also concatenate labels, width, fish size bin, reef slope, visibility, current, relative depth, and tide
  #
  # obs_agg_biomass_long <- calculate_obs_biomass_long(obs) %>%
  #   dplyr::mutate_if(is.numeric, round) %>%
  #   dplyr::mutate(obs = as.character(obs))
  #
  # obs_agg_concatenate_long <- obs %>%
  #   dplyr::group_by(fake_sample_unit_id) %>%
  #   dplyr::summarise(dplyr::across(c(label, size_bin, transect_width, reef_slope, visibility, current, relative_depth, tide), ~ paste(sort(unique(.x)), collapse = ", ")),
  #     .groups = "drop"
  #   ) %>%
  #   tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
  #
  # obs_agg_for_su_comparison <- obs_agg_biomass_long %>%
  #   dplyr::bind_rows(obs_agg_concatenate_long)
  #
  # sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros) %>%
  #   dplyr::mutate_if(is.numeric, round) %>%
  #   dplyr::mutate(su = as.character(su)) %>%
  #   dplyr::bind_rows(sus_minus_zeros %>%
  #     dplyr::select(fake_sample_unit_id, label, size_bin, transect_width, reef_slope, visibility, current, relative_depth, tide) %>%
  #     tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su"))
  #
  # test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Big/small fish fishbelt sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "507d1af9-edbd-417e-a65c-350f8bba1299"

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "fishbelt", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate SUs to sample events
  # Calculate biomass_kgha_avg, biomass_kgha_trophic_group_avg, and biomgass_kgha_fish_family_avg
  sus_agg_for_se_comparison <- calculate_sus_biomass_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_biomass_avg_long(ses)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

## Missing sample unit cases ----

test_that("Fishbelt sample unit aggregation is the same as manually aggregating observations, cases where some sample units were previously missing", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "02e6915c-1c64-4d2c-bac0-326b560415a2"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations") %>%
    construct_fake_sample_unit_id()

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  sus_minus_zeros <- sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus_minus_zeros)

  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family

  obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)

  # Check that values match

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)

  project_id <- "170e7182-700a-4814-8f1e-45ee1caf3b44"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations") %>%
    construct_fake_sample_unit_id()

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  sus_minus_zeros <- sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus_minus_zeros)

  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family

  obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)

  # Check that values match

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

## Deep/shallow ----

test_that("Deep/shallow fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # TODO
  # project_id <- "75ef7a5a-c770-4ca6-b9f8-830cab74e425"
  #
  # obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")
  #
  # sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")
  #
  # obs <- obs %>%
  #   construct_fake_sample_unit_id()
  #
  # # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons
  #
  # sus_minus_zeros <- sus %>%
  #   dplyr::filter(biomass_kgha != 0) %>%
  #   construct_fake_sample_unit_id()
  #
  # # Check first that there are the same number of fake SUs as real SUs
  # test_n_fake_sus(obs, sus_minus_zeros)
  #
  # # Doing this confirms that even if a set of observations are at the same site, same date, transect, and transect length, if they have different depths (deep/shallow cases), they are treated as *different* sample units and not combined
  # # To triple check: for every site/sample date/transect number/transect length, the number of unique IDs should be the same as the number of unique depths (and both the same as the number of fake IDs)
  # sus_depth_different_sample_unit <- sus_minus_zeros %>%
  #   dplyr::group_by(site, sample_date, transect_number, transect_length) %>%
  #   dplyr::summarise(
  #     n_depths = dplyr::n_distinct(depth),
  #     n_ids = dplyr::n_distinct(sample_unit_ids),
  #     n_fake_ids = dplyr::n_distinct(fake_sample_unit_id),
  #     match_depth_ids = n_depths == n_ids,
  #     match_depth_fake_ids = n_depths == n_fake_ids,
  #     .groups = "drop"
  #   )
  #
  # expect_true(all(sus_depth_different_sample_unit[["match_depth_ids"]]))
  # expect_true(all(sus_depth_different_sample_unit[["match_depth_fake_ids"]]))
  #
  # # Aggregate observations to sample units
  # # Calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family
  # # Do NOT concatenate any fields
  #
  # obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)
  #
  # sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)
  #
  # test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Deep/shallow fishbelt sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "75ef7a5a-c770-4ca6-b9f8-830cab74e425"

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "fishbelt", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate observations to sample events
  # Calculate biomass_kgha_avg, biomass_kgha_trophic_group_avg, and biomass_kgha_fish_family_avg

  sus_agg_for_se_comparison <- calculate_sus_biomass_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_biomass_avg_long(ses)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

# Benthic LIT ----

test_that("Benthic LIT sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  obs <- mermaid_get_project_data(project_id, "benthiclit", "observations")

  sus <- mermaid_get_project_data(project_id, "benthiclit", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_benthic_category

  obs_agg_for_su_comparison <- calculate_lit_obs_percent_cover_long(obs)

  sus_for_su_comparison <- aggregate_sus_percent_cover_long(sus)

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Benthic LIT sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  sus <- mermaid_get_project_data(project_id, "benthiclit", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "benthiclit", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_benthic_category_avg and depth_avg

  sus_agg_for_se_comparison <- calculate_sus_percent_cover_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_percent_cover_avg_long(ses)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

# Benthic PIT -----

test_that("Benthic PIT sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "5679ef3d-bafc-453d-9e1a-a4b282a8a997"

  obs <- mermaid_get_project_data(project_id, "benthicpit", "observations")

  sus <- mermaid_get_project_data(project_id, "benthicpit", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_benthic_category
  # Do this by getting the length for each benthic category (sum of interval_size) divided by the total length (transect_length)

  obs_agg_for_su_comparison <- calculate_pit_obs_percent_cover_long(obs)

  sus_for_su_comparison <- aggregate_sus_percent_cover_long(sus)

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Benthic PIT sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "5679ef3d-bafc-453d-9e1a-a4b282a8a997"

  sus <- mermaid_get_project_data(project_id, "benthicpit", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "benthicpit", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_benthic_category_avg and depth_avg

  sus_agg_for_se_comparison <- calculate_sus_percent_cover_avg_long(sus)

  ses_for_se_comparison <- aggregate_ses_percent_cover_avg_long(ses)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

## Missing sample unit cases

test_that("Benthic PIT sample unit aggregation is the same as manually aggregating observations, cases where some sample units were previously missing", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "e1efb1e0-0af8-495a-9c69-fddcdba11c14"

  obs <- mermaid_get_project_data(project_id, "benthicpit", "observations")

  sus <- mermaid_get_project_data(project_id, "benthicpit", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_benthic_category
  # Do this by getting the length for each benthic category (sum of interval_size) divided by the total length (transect_length)

  obs_agg_for_su_comparison <- calculate_pit_obs_percent_cover_long(obs)

  sus_for_su_comparison <- aggregate_sus_percent_cover_long(sus)

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

# Habitat Complexity -----

test_that("Habitat complexity sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"

  obs <- mermaid_get_project_data(project_id, "habitatcomplexity", "observations")

  sus <- mermaid_get_project_data(project_id, "habitatcomplexity", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  test_n_fake_sus(obs, sus)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to score_avg

  obs_agg_for_su_comparison <- calculate_obs_score_long(obs)

  sus_for_su_comparison <- unpack_sus_score_long(sus, obs_agg_for_su_comparison)

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Habitat complexity sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"

  sus <- mermaid_get_project_data(project_id, "habitatcomplexity", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "habitatcomplexity", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to score_avg_avg and depth_avg

  sus_agg_for_se_comparison <- calculate_sus_score_avg_long(sus)

  ses_for_se_comparison <- unpack_ses_score_avg_long(ses, sus_agg_for_se_comparison)

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

# Bleaching -----

test_that("NULL values for percent cover in bleaching observations come through properly as NAs", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  res <- mermaid_get_project_data("2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "bleaching", "observations")[["percent_cover"]]

  expect_identical(res[["percent_soft"]], NA)
})

test_that("Bleaching sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  obs <- mermaid_get_project_data(project_id, "bleaching", "observations")

  obs_colonies_bleached <- obs[["colonies_bleached"]] %>%
    construct_bleaching_fake_sample_unit_id()

  obs_percent_cover <- obs[["percent_cover"]] %>%
    construct_bleaching_fake_sample_unit_id()

  sus <- mermaid_get_project_data(project_id, "bleaching", "sampleunits")

  # Check first that there are the same number of fake SUs as real SUs
  obs_sample_units <- obs_colonies_bleached %>%
    dplyr::distinct(sample_unit_id, fake_sample_unit_id) %>%
    dplyr::bind_rows(obs_percent_cover %>%
      dplyr::distinct(sample_unit_id, fake_sample_unit_id))

  test_n_fake_sus(obs_sample_units, sus)

  # Check that su.sample_unit_ids contains obs.sample_unit_id for cases where they have the same fake_sample_unit_id

  sus_ids <- sus %>%
    construct_bleaching_fake_sample_unit_id() %>%
    dplyr::select(fake_sample_unit_id, sample_unit_id = sample_unit_ids) %>%
    tidyr::separate_rows(sample_unit_id, sep = "; ") %>%
    dplyr::arrange(fake_sample_unit_id, sample_unit_id)

  obs_ids <- obs_sample_units %>%
    dplyr::select(fake_sample_unit_id, sample_unit_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(fake_sample_unit_id, sample_unit_id)

  expect_identical(sus_ids, obs_ids)

  # Aggregate observations to sample units

  # Aggregate colonies_bleached first - count_total, count_genera, percent_normal, percent_pale, percent_bleached
  obs_colonies_bleached_agg <- calculate_obs_colonies_long(obs_colonies_bleached)

  # Aggregate percent_cover - quadrat_count, percent_hard_avg, percent_soft_avg, percent_algae_avg
  obs_percent_cover_agg <- calculate_obs_percent_cover_long(obs_percent_cover)

  # Also concatenate labels, width, fish size bin, reef slope, visibility, current, relative depth, and tide
  obs_agg_concatenate_long <- obs_percent_cover %>%
    dplyr::bind_rows(obs_colonies_bleached) %>%
    dplyr::select(fake_sample_unit_id, label, visibility, current, relative_depth, tide) %>%
    dplyr::distinct() %>%
    dplyr::group_by(fake_sample_unit_id) %>%
    dplyr::summarise(dplyr::across(c(label, visibility, current, relative_depth, tide), ~ paste(sort(unique(.x)), collapse = ", ")),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")

  obs_agg_for_su_comparison <- obs_colonies_bleached_agg %>%
    dplyr::bind_rows(obs_percent_cover_agg) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(obs = as.character(obs)) %>%
    dplyr::bind_rows(obs_agg_concatenate_long)

  sus_for_su_comparison <- unpack_sus_bleaching_long(sus, obs_agg_for_su_comparison) %>%
    dplyr::mutate(su = dplyr::coalesce(su, ""))

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
})

test_that("Bleaching sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  sus <- mermaid_get_project_data(project_id, "bleaching", "sampleunits")

  sus <- sus %>%
    construct_fake_sample_event_id()

  ses <- mermaid_get_project_data(project_id, "bleaching", "sampleevents")

  # Check first that there are the same number of fake SEs as real SEs
  test_n_fake_ses(sus, ses)

  # Aggregate SUs to SEs
  # depth_avg, quadrat_size_avg, count_total_avg, count_genera_avg, percent_normal_avg, percent_pale_avg, percent_bleached_avg, quadrat_count_avg, percent_hard_avg_avg, percent_soft_avg_avg, percent_algae_avg_avg

  sus_agg_for_se_comparison <- calculate_sus_bleaching_long(sus)

  ses_for_se_comparison <- unpack_sus_bleaching_avg_long(ses, sus_agg_for_se_comparison)

  # Check that values match

  test_sus_vs_ses_agg(sus_agg_for_se_comparison, ses_for_se_comparison)
})

# Benthic PQT ----

test_that("mermaid_get_project_data for benthicpqt returns a data frame with the correct names", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2c0c9857-b11c-4b82-b7ef-e9b383d1233c", method = "benthicpqt", data = "all")
  expect_true(all(project_data_test_columns[["benthicpqts/observations"]] %in% names(output[["observations"]])))
  expect_true(all(project_data_test_columns[["benthicpqts/sampleunits"]] %in% names(output[["sampleunits"]])))
  expect_true(all(project_data_test_columns[["benthicpqts/sampleevents"]] %in% names(output[["sampleevents"]])))
  expect_true(any(stringr::str_starts(names(output[["sampleunits"]]), project_data_df_columns_list_names[["benthicpqts/sampleunits"]])))
  expect_true(any(stringr::str_starts(names(output[["sampleevents"]]), project_data_df_columns_list_names[["benthicpqts/sampleevents"]])))
})

# Covariates ----

test_that("mermaid_get_project_data with covariates = FALSE (the default) doesn't return any covars", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects()
  # output <- mermaid_get_project_data(p, "all", "all", limit = 1)

  # TODO
  expect_true(FALSE)
})


test_that("mermaid_get_project_data with covariates = TRUE returns covars, all the way down", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # No data, still contains cols
  p <- "173c2353-3ee3-49d1-b08a-a6bdeca2b52c"
  output <- mermaid_get_project_data(p, "all", "all", limit = 1, covariates = TRUE)
  output_t <- output %>%
    purrr::transpose()
  purrr::walk(
    output_t[["sampleunits"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["sampleevents"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]][names(output_t[["observations"]]) != "bleaching"],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]][["bleaching"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "all", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "observations", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output)))
  output <- mermaid_get_project_data(p, "bleaching", "all", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output[["sampleunits"]])))
  expect_true(all(covars_cols %in% names(output[["sampleevents"]])))
  purrr::walk(
    output[["observations"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "bleaching", "observations", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )

  # One project, contains cols
  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  output <- mermaid_get_project_data(p, c("fishbelt", "habitatcomplexity"), "all", limit = 1, covariates = TRUE)
  output_t <- output %>%
    purrr::transpose()
  purrr::walk(
    output_t[["sampleunits"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["sampleevents"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "all", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "observations", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output)))

  p <- "170e7182-700a-4814-8f1e-45ee1caf3b44"
  output <- mermaid_get_project_data(p, c("fishbelt", "benthicpit"), "all", limit = 1, covariates = TRUE)
  output_t <- output %>%
    purrr::transpose()
  purrr::walk(
    output_t[["sampleunits"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["sampleevents"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "all", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "observations", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output)))

  p <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"
  output <- mermaid_get_project_data(p, c("bleaching", "benthiclit"), "all", limit = 1, covariates = TRUE)
  output_t <- output %>%
    purrr::transpose()
  purrr::walk(
    output_t[["sampleunits"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["sampleevents"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  expect_true(all(covars_cols %in% names(output_t[["observations"]][["benthiclit"]])))
  purrr::walk(
    output_t[["observations"]][["bleaching"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "benthiclit", "all", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "benthiclit", "observations", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output)))

  # Multiple projects, contains cols
  p <- c(
    "02e6915c-1c64-4d2c-bac0-326b560415a2",
    "170e7182-700a-4814-8f1e-45ee1caf3b44",
    "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b",
    "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"
  )
  output <- mermaid_get_project_data(p, "all", "all", limit = 1, covariates = TRUE)
  output_t <- output %>%
    purrr::transpose()
  purrr::walk(
    output_t[["sampleunits"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["sampleevents"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]][names(output_t[["observations"]]) != "bleaching"],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  purrr::walk(
    output_t[["observations"]][["bleaching"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "all", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "fishbelt", "observations", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output)))
  output <- mermaid_get_project_data(p, "bleaching", "all", limit = 1, covariates = TRUE)
  expect_true(all(covars_cols %in% names(output[["sampleunits"]])))
  expect_true(all(covars_cols %in% names(output[["sampleevents"]])))
  purrr::walk(
    output[["observations"]],
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
  output <- mermaid_get_project_data(p, "bleaching", "observations", limit = 1, covariates = TRUE)
  purrr::walk(
    output,
    ~ expect_true(all(covars_cols %in% names(.x)))
  )
})

# _by_ removal ----

test_that("All expanded columns that formerly had _by_ in them are properly pulled down", {
  p <- mermaid_get_my_projects()
  cols <- project_data_df_columns_list %>%
    purrr::map_dfr(dplyr::as_tibble, .id = "method_data") %>%
    tidyr::separate(method_data, into = c("method", "data"), sep = "/") %>%
    dplyr::mutate(method = dplyr::case_when(
      method == "beltfishes" ~ "fishbelt",
      stringr::str_starts(method, "benthic") ~ stringr::str_remove(method, "s")
    ))

  cols %>%
    dplyr::distinct(method, data) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    split(.$id) %>%
    purrr::map(
      function(x) {
        res <- mermaid_get_project_data(p, x$method, x$data)
        col <- x %>%
          dplyr::left_join(cols, by = c("method", "data")) %>%
          dplyr::pull(value)

        purrr::walk(
          col,
          function(col) {
            expect_true(any(stringr::str_starts(names(res), col)))
          }
        )
      }
    )
})

# Standard Deviations ----

test_that("Every column ending in _avg has an _sd column accounted for in col selection", {
  cols_by_endpoint <- project_data_columns %>%
    purrr::map_df(dplyr::as_tibble, .id = "endpoint")

  avg_cols <- cols_by_endpoint %>%
    dplyr::filter(stringr::str_ends(value, "_avg"))

  avg_cols_sd_counterpart <- avg_cols %>%
    dplyr::mutate(value = stringr::str_replace(value, "_avg$", "_sd"))

  sd_counterpart_matched <- avg_cols_sd_counterpart %>%
    dplyr::inner_join(cols_by_endpoint, by = c("endpoint", "value"))

  expect_identical(sd_counterpart_matched, avg_cols_sd_counterpart)
})

test_that("Fishbelt - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "fishbelt"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})

test_that("Benthic LIT - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "benthiclit"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})

test_that("Benthic PIT - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "benthicpit"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})

test_that("Benthic PQT - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "benthicpqt"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})

test_that("Habtitat Complexity - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "habitatcomplexity"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})


test_that("Bleaching - standard deviations calculated in API are the same as SDs calculated manually", {
  method <- "bleaching"
  sd_cols <- get_sd_cols(method)
  p <- mermaid_get_my_projects()

  ## Sample units ----
  # sd_cols %>%
    # check_agg_sd_vs_agg_from_raw(method, "sampleunits")

  ## Sample events ----
  sd_cols %>%
    check_agg_sd_vs_agg_from_raw(method, "sampleevents")

  # NOTE: assumption is that NAs are getting treated as 0s
  # e.g. if some SUs in an SE have data for fish_family_balistidae, but one does not, the one that does not is a 0, not an NA that is removed
})
