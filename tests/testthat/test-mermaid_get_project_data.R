test_that("covariates produces a warning", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_message(
    mermaid_get_project_data("170e7182-700a-4814-8f1e-45ee1caf3b44", method = "benthicpit", data = "sampleunits", limit = 1, covariates = TRUE),
    "deprecated"
  )
})

test_that("mermaid_get_project_data returns a data frame", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("170e7182-700a-4814-8f1e-45ee1caf3b44", method = "benthicpit", data = "sampleunits", limit = 1)
  expect_true(nrow(output) == 1)
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
  expect_named(output, methods)
  purrr::walk(output, expect_named, c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data with 'bleaching' method and 'observations' data returns a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "observations", limit = 1)
  expect_named(output, c("colonies_bleached", "percent_cover"))
})

test_that("mermaid_get_project_data with 'bleaching' method and multiple values for `data` (including 'observations') returns the 'observations' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "all", limit = 1)
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "observations", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "observations", "sampleunits"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))
})

test_that("mermaid_get_project_data with multiple `methods` (including 'bleaching') returns the 'bleaching' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("fishbelt", "bleaching"), "observations", limit = 1)
  expect_named(output, c("fishbelt", "bleaching"))
  expect_named(output[["bleaching"]], c("colonies_bleached", "percent_cover"))

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

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "sampleunits"))
})

test_that("mermaid_get_project_data with multiple methods returns a list with multiple elements in the same order that they were supplied", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("bleaching", "benthicpit"), "sampleevents", limit = 1)
  expect_named(output, c("bleaching", "benthicpit"))

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("benthicpit", "bleaching"), "sampleevents", limit = 1)
  expect_named(output, c("benthicpit", "bleaching"))
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
  # 10 * count * biomass_constant_a * (size * biomass_constant_c) ^ biomass_constant_b / (transect_len_surveyed * width)
  # In the mixed width case, the width depends on the size
  # In this project, the width is: 2m if size < 10cm, 5m if size >= 10cm

  obs_biomass_calc <- obs %>%
    dplyr::mutate(
      width = dplyr::case_when(
        size < 10 ~ 2,
        size >= 10 ~ 5
      ),
      biomass_kgha_calc = 10 * count * biomass_constant_a * (size * biomass_constant_c)^biomass_constant_b / (transect_len_surveyed * width),
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

  # Check that su.sample_unit_ids contains obs.sample_unit_id for cases where they have the same fake_sample_unit_id

  sus_ids <- sus_minus_zeros %>%
    dplyr::select(fake_sample_unit_id, sample_unit_id = sample_unit_ids) %>%
    # tidyr::separate_rows(sample_unit_id, sep = "; ") %>%
    # Now separated by "," not "; "
    tidyr::separate_rows(sample_unit_id, sep = ",") %>%
    dplyr::arrange(fake_sample_unit_id, sample_unit_id)

  obs_ids <- obs %>%
    dplyr::select(fake_sample_unit_id, sample_unit_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(fake_sample_unit_id, sample_unit_id)

  expect_identical(sus_ids, obs_ids)

  # Check that every sample unit has a big/small transect
  # This means that each "fake" sample unit id has 2 (pseudo) sample unit ids
  expect_equal(sus_ids %>%
    dplyr::count(fake_sample_unit_id) %>%
    dplyr::pull(n) %>%
    unique(), 2)

  # Also means that every set of observations is either BF or SF, and has a corresponding SF/BF
  expect_identical(
    obs %>%
      dplyr::distinct(fake_sample_unit_id, label) %>%
      dplyr::group_by(fake_sample_unit_id) %>%
      dplyr::summarise(
        label = paste0(sort(label), collapse = ","),
        .groups = "drop"
      ) %>%
      dplyr::pull(label) %>%
      unique(),
    "BF,SF"
  )

  # Aggregate observations to sample units
  # Calculate biomass_kgha, biomass_kgha_trophic_group, and biomass_kgha_fish_family
  # Also concatenate labels, width, fish size bin, reef slope, visibility, current, relative depth, and tide

  obs_agg_biomass_long <- calculate_obs_biomass_long(obs) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate(obs = as.character(obs))

  obs_agg_concatenate_long <- obs %>%
    dplyr::group_by(fake_sample_unit_id) %>%
    dplyr::summarise(dplyr::across(c(label, size_bin, transect_width, reef_slope, visibility, current, relative_depth, tide), ~ paste(sort(unique(.x)), collapse = ", ")),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::bind_rows(sus_minus_zeros %>%
      dplyr::select(fake_sample_unit_id, tidyselect::starts_with("biomass_kgha")) %>%
      tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su") %>%
      dplyr::mutate(name = stringr::str_remove(name, "biomass_kgha_")))

  obs_agg_for_su_comparison <- obs_agg_biomass_long %>%
    dplyr::bind_rows(obs_agg_concatenate_long) %>%
    dplyr::filter(name %in% sus_for_su_comparison[["name"]]) %>%
    dplyr::mutate(obs = as.numeric(obs))

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
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

  project_id <- "75ef7a5a-c770-4ca6-b9f8-830cab74e425"

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

  # Doing this confirms that even if a set of observations are at the same site, same date, transect, and transect length, if they have different depths (deep/shallow cases), they are treated as *different* sample units and not combined
  # To triple check: for every site/sample date/transect number/transect length, the number of unique IDs should be the same as the number of unique depths (and both the same as the number of fake IDs)
  sus_depth_different_sample_unit <- sus_minus_zeros %>%
    dplyr::group_by(site, sample_date, transect_number, transect_len_surveyed) %>%
    dplyr::summarise(
      n_depths = dplyr::n_distinct(depth),
      n_ids = dplyr::n_distinct(sample_unit_ids),
      n_fake_ids = dplyr::n_distinct(fake_sample_unit_id),
      match_depth_ids = n_depths == n_ids,
      match_depth_fake_ids = n_depths == n_fake_ids,
      .groups = "drop"
    )

  expect_true(all(sus_depth_different_sample_unit[["match_depth_ids"]]))
  expect_true(all(sus_depth_different_sample_unit[["match_depth_fake_ids"]]))

  # Aggregate observations to sample units
  # Calculate biomass_kgha, biomass_kgha_by_trophic_group, and biomass_kgha_by_fish_family
  # Do NOT concatenate any fields

  obs_agg_for_su_comparison <- calculate_obs_biomass_long(obs)

  sus_for_su_comparison <- aggregate_sus_biomass_long(sus_minus_zeros)

  test_obs_vs_sus_agg(obs_agg_for_su_comparison, sus_for_su_comparison)
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
  # Do this by getting the length for each benthic category (sum of interval_size) divided by the total length (transect_len_surveyed)

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
  # Do this by getting the length for each benthic category (sum of interval_size) divided by the total length (transect_len_surveyed)

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

  expect_true(any(res[["percent_soft"]] %>% is.na()))
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
    tidyr::separate_rows(sample_unit_id, sep = ",") %>%
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
    # Remove leading ", " from collapse on server
    dplyr::mutate(
      su = dplyr::case_when(
        stringr::str_starts(su, ", ") ~ stringr::str_remove(su, ", "),
        TRUE ~ su
      ),
      su = dplyr::case_when(
        name %in% c("label", "visibility", "current", "relative_depth", "tide") ~ dplyr::coalesce(su, ""),
        TRUE ~ su
      )
    )

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

# Inverts ----

test_that("Inverts sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "bacd3529-e0f4-40f4-a089-992c5bd5cc02"
  obs <- mermaid_get_project_data(p, "macroinvertebrate", "observations")
  su <- mermaid_get_project_data(p, "macroinvertebrate", "sampleunits")

  obs_to_su <- obs %>%
    dplyr::select(
      site, transect_number, sample_date, invert_class, invert_order, invert_family,
      invert_genus, invert_taxon, invert_group_of_interest,
      size, count, density_indha
    ) %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      multiple_goi = stringr::str_detect(invert_group_of_interest, ",")
    ) %>%
    tidyr::separate_rows(invert_group_of_interest, sep = ", ") %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      n_goi = dplyr::n_distinct(invert_group_of_interest),
      density_ind_ha_by_goi = density_indha / n_goi
    ) %>%
    dplyr::group_by(site, sample_date, transect_number, invert_group_of_interest) %>%
    dplyr::summarise(dplyr::across(density_ind_ha_by_goi, sum),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = invert_group_of_interest, values_from = density_ind_ha_by_goi,
      names_prefix = "density_indha_group_interest_"
    ) %>%
    dplyr::arrange(site, sample_date, transect_number) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("density_indha_group_interest"), round))

  names(obs_to_su) <- names(obs_to_su) %>%
    stringr::str_remove_all("'") %>%
    snakecase::to_snake_case()

  su_relevant <- su %>%
    dplyr::select(dplyr::any_of(names(obs_to_su))) %>%
    dplyr::arrange(site, sample_date, transect_number) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("density_indha_group_interest"), round))

  expect_identical(obs_to_su, su_relevant)
})

test_that("Inverts sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "bacd3529-e0f4-40f4-a089-992c5bd5cc02"
  su <- mermaid_get_project_data(p, "macroinvertebrate", "sampleunits")
  se <- mermaid_get_project_data(p, "macroinvertebrate", "sampleevents")

  su_to_se_by_goi <- su %>%
    dplyr::select(site, sample_date, transect_number, dplyr::starts_with("density_indha_group")) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("density_indha")) %>%
    dplyr::mutate(value = dplyr::coalesce(value, 0)) %>%
    dplyr::group_by(site, sample_date, name) %>%
    dplyr::summarise(
      avg = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      sd = dplyr::coalesce(sd, 0),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(c(avg, sd), names_to = "stat") %>%
    dplyr::mutate(
      name = stringr::str_remove(name, "density_indha_group_interest_"),
      name = paste0("density_indha_group_interest_", stat, "_", name),
      value = round(value)
    ) %>%
    dplyr::select(-stat) %>%
    dplyr::arrange(site, sample_date, name)

  se_goi <- se %>%
    dplyr::select(site, sample_date, contains("group_interest")) %>%
    tidyr::pivot_longer(dplyr::contains("group_interest")) %>%
    dplyr::mutate(
      value = round(value),
      value = dplyr::coalesce(value, 0)
    ) %>%
    dplyr::arrange(site, sample_date, name)

  expect_identical(su_to_se_by_goi, se_goi)

  su_to_se_overall_summary <- su %>%
    dplyr::select(site, sample_date, density_indha) %>%
    dplyr::group_by(site, sample_date) %>%
    dplyr::summarise(
      density_indha_avg = mean(density_indha),
      density_indha_sd = sd(density_indha),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c(density_indha_avg, density_indha_sd), round)) %>%
    dplyr::arrange(site, sample_date)

  se_overall_summary <- se %>%
    dplyr::select(dplyr::all_of(names(su_to_se_overall_summary))) %>%
    dplyr::mutate(dplyr::across(c(density_indha_avg, density_indha_sd), round)) %>%
    dplyr::arrange(site, sample_date)

  expect_identical(su_to_se_overall_summary, se_overall_summary)
})
