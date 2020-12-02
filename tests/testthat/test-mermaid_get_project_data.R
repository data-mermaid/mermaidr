library(dplyr)
library(tidyr)

# Function to construct a fake sample unit, which combines site, sample date, depth, transect number, and transect length to make an ID
construct_fake_sample_unit_id <- function(data) {
  data %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{transect_number}_{transect_length}"))
}

# Function to construct a fake sample unit, which combines site and date to make an ID

construct_fake_sample_event_id <- function(data) {
  data %>%
    mutate(fake_sample_event_id = glue::glue("{site}_{sample_date}_{management}"))
}

test_that("mermaid_get_project_data returns a data frame with the correct names", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects()
  output <- mermaid_get_project_data(p, method = "benthicpit", data = "sampleunits", limit = 1)
  expect_named(output, project_data_columns[["benthicpits/sampleunits"]])
  expect_true(nrow(output) >= 1)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_project_data allows multiple methods", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = c("fishbelt", "benthicpit", "benthiclit"), data = "sampleunits", limit = 1)
  expect_named(output, c("fishbelt", "benthicpit", "benthiclit"))
})

test_that("mermaid_get_project_data allows multiple forms of data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = "fishbelt", data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data allows multiple methods and multiple forms of data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = c("fishbelt", "benthicpit"), data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_named(output, c("fishbelt", "benthicpit"))
  expect_named(output[["fishbelt"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpit"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpit"]][["sampleunits"]], project_data_columns[["benthicpits/sampleunits"]])
  expect_named(output[["fishbelt"]][["observations"]], project_data_columns[["beltfishes/obstransectbeltfishes"]])
})

test_that("mermaid_get_project_data errors if passed a wrong method or data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 1)
  expect_error(mermaid_get_project_data(p, method = "beltfishs", data = "sampleunits"), "one of")
  expect_error(mermaid_get_project_data(p, method = "benthicpits", data = "samplevents"), "one of")
})

test_that("mermaid_get_project_data setting 'all' works", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = "all", data = "all", limit = 1)
  expect_named(output, c("fishbelt", "benthiclit", "benthicpit", "bleaching", "habitatcomplexity"))
  expect_named(output[["fishbelt"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpit"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthiclit"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["habitatcomplexity"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["bleaching"]], c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data with 'bleaching' method and 'observations' data returns a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "observations", limit = 1)
  expect_named(output, c("colonies_bleached", "percent_cover"))
  expect_named(output[["colonies_bleached"]], project_data_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["percent_cover"]], project_data_columns[["bleachingqcs/obsquadratbenthicpercents"]])
})

test_that("mermaid_get_project_data with 'bleaching' method and multiple values for `data` (including 'observations') returns the 'observations' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", "all", limit = 1)
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["observations"]][["colonies_bleached"]], project_data_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["observations"]][["percent_cover"]], project_data_columns[["bleachingqcs/obsquadratbenthicpercents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "observations", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "observations", "sampleunits"))
  expect_named(output[["observations"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["observations"]][["colonies_bleached"]], project_data_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["observations"]][["percent_cover"]], project_data_columns[["bleachingqcs/obsquadratbenthicpercents"]])
})

test_that("mermaid_get_project_data with multiple `methods` (including 'bleaching') returns the 'bleaching' element as a list with elements 'colonies_bleached' and 'percent_cover'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("fishbelt", "bleaching"), "observations", limit = 1)
  expect_named(output, c("fishbelt", "bleaching"))
  expect_named(output[["bleaching"]], c("colonies_bleached", "percent_cover"))
  expect_named(output[["bleaching"]][["colonies_bleached"]], project_data_columns[["bleachingqcs/obscoloniesbleacheds"]])
  expect_named(output[["bleaching"]][["percent_cover"]], project_data_columns[["bleachingqcs/obsquadratbenthicpercents"]])

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
  expect_named(output[["sampleunits"]], project_data_columns[["bleachingqcs/sampleunits"]])
  expect_named(output[["sampleevents"]], project_data_columns[["bleachingqcs/sampleevents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "bleaching", c("sampleevents", "sampleunits"), limit = 1)
  expect_named(output, c("sampleevents", "sampleunits"))
  expect_named(output[["sampleunits"]], project_data_columns[["bleachingqcs/sampleunits"]])
  expect_named(output[["sampleevents"]], project_data_columns[["bleachingqcs/sampleevents"]])
})

test_that("mermaid_get_project_data with multiple methods returns a list with multiple elements in the same order that they were supplied", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("bleaching", "benthicpit"), "sampleevents", limit = 1)
  expect_named(output, c("bleaching", "benthicpit"))
  expect_named(output[["bleaching"]], project_data_columns[["bleachingqcs/sampleevents"]])
  expect_named(output[["benthicpit"]], project_data_columns[["benthicpits/sampleevents"]])

  output <- mermaid_get_project_data("2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", c("benthicpit", "bleaching"), "sampleevents", limit = 1)
  expect_named(output, c("benthicpit", "bleaching"))
  expect_named(output[["bleaching"]], project_data_columns[["bleachingqcs/sampleevents"]])
  expect_named(output[["benthicpit"]], project_data_columns[["benthicpits/sampleevents"]])
})

# Testing aggregation views ----

# Fishbelt ----

# Vanilla fishbelt ----

test_that("Vanilla fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")

  sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

  obs <- obs %>%
    construct_fake_sample_unit_id()

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  sus_minus_zeros <- sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  expect_equal(
    sus_minus_zeros %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha and biomass_kgha_by_trophic_group

  obs_agg <- obs %>%
    group_by(fake_sample_unit_id, trophic_group) %>%
    summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    ungroup() %>%
    pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus_minus_zeros %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate sample units to sample events - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc - but will want to check these in the other fishbelts!
  # Just aggregate straight up to calculate depth_avg, biomass_kgha_avg and biomass_kgha_by_trophic_group_avg, sample_unit_count

  biomass_kgha_by_trophic_group_cols <- sus %>%
    pull(biomass_kgha_by_trophic_group) %>%
    names()

  # In API, all are rounded to 2 decimal places - but just round to 0, because of some weird rounding issues in R
  sus_agg_for_se_comparison <- sus %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(sample_event_id, all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(biomass_kgha_by_trophic_group_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})

# Variable widths ----

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
  expect_equal(
    sus_minus_zeros %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )
  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha and biomass_kgha_by_trophic_group

  obs_agg <- obs %>%
    # TODO
    mutate(trophic_group = coalesce(trophic_group, "other")) %>%
    group_by(fake_sample_unit_id, trophic_group) %>%
    summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    ungroup() %>%
    pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus_minus_zeros %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate sample units to sample events - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc - but will want to check these in the other fishbelts!
  # Just aggregate straight up to calculate depth_avg, biomass_kgha_avg and biomass_kgha_by_trophic_group_avg, sample_unit_count

  biomass_kgha_by_trophic_group_cols <- sus %>%
    pull(biomass_kgha_by_trophic_group) %>%
    names()

  # In API, all are rounded to 2 decimal places - but just round to 0, because of some weird rounding issues in R
  sus_agg_for_se_comparison <- sus %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(sample_event_id, all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(biomass_kgha_by_trophic_group_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = abs(se - su) <= 1,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})

# Big/small fish ----

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
  expect_equal(
    sus_minus_zeros %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )

  # Check that su.sample_unit_ids contains obs.sample_unit_id for cases where they have the same fake_sample_unit_id

  sus_ids <- sus_minus_zeros %>%
    select(fake_sample_unit_id, sample_unit_id = sample_unit_ids) %>%
    separate_rows(sample_unit_id, sep = "; ") %>%
    arrange(fake_sample_unit_id, sample_unit_id)

  obs_ids <- obs %>%
    select(fake_sample_unit_id, sample_unit_id) %>%
    distinct() %>%
    arrange(fake_sample_unit_id, sample_unit_id)

  expect_identical(sus_ids, obs_ids)

  # Check that every sample unit has a big/small transect
  # This means that each "fake" sample unit id has 2 (pseudo) sample unit ids
  expect_equal(sus_ids %>%
    count(fake_sample_unit_id) %>%
      pull(n) %>%
      unique(), 2)

  # Also means that every set of observations is either BF or SF, and has a corresponding SF/BF
  expect_identical(
    obs %>%
    distinct(fake_sample_unit_id, label) %>%
    group_by(fake_sample_unit_id) %>%
    summarise(label = paste0(sort(label), collapse = ","),
              .groups = "drop") %>%
    pull(label) %>%
    unique(),
  "BF,SF")

  # Aggregate observations to sample units
  # Calculate biomass_kgha and biomass_kgha_by_trophic group
  # Also concatenate labels, width, fish size bin, reef slope, visibility, current, relative depth, and tide
  obs_agg_biomass <- obs %>%
    mutate(trophic_group = coalesce(trophic_group, "other")) %>%
    group_by(fake_sample_unit_id, trophic_group) %>%
    summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    ungroup() %>%
    pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  obs_agg_concatenate <- obs %>%
    group_by(fake_sample_unit_id) %>%
    summarise(across(c(label, size_bin, transect_width, reef_slope, visibility, current, relative_depth, tide), ~ paste(sort(unique(.x)), collapse = ", ")),
      .groups = "drop"
    )

  obs_agg <- obs_agg_biomass %>%
    left_join(obs_agg_concatenate, by = "fake_sample_unit_id")

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus_minus_zeros %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate observations to sample events
  # Calculate biomass_kgha_avg and biomass_kgha_by_trophic_group_avg

  biomass_kgha_by_trophic_group_cols <- sus %>%
    pull(biomass_kgha_by_trophic_group) %>%
    names()

  # In API, all are rounded to 2 decimal places - but just round to 0, because of some weird rounding issues in R
  sus_agg_for_se_comparison <- sus %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(sample_event_id, all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(biomass_kgha_by_trophic_group_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = abs(se - su) <= 1,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})

# Deep/shallow ----

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
  expect_equal(
    sus_minus_zeros %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )

  # Aggregate observations to sample units
  # Calculate biomass_kgha and biomass_kgha_by_trophic group
  # Also concatenate labels, width, fish size bin, reef slope, visibility, current, relative depth, and tide

  obs_agg_biomass <- obs %>%
    mutate(trophic_group = coalesce(trophic_group, "other")) %>%
    group_by(fake_sample_unit_id, trophic_group) %>%
    summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    ungroup() %>%
    pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  obs_agg_concatenate <- obs %>%
    group_by(fake_sample_unit_id) %>%
    summarise(across(c(label, size_bin, transect_width, reef_slope, visibility, current, relative_depth, tide), ~ paste(sort(unique(.x)), collapse = ", ")),
              .groups = "drop"
    )

  obs_agg <- obs_agg_biomass %>%
    left_join(obs_agg_concatenate, by = "fake_sample_unit_id")

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs") %>%
    mutate(obs = ifelse(obs == "", NA_character_, obs))

  sus_for_su_comparison <- sus_minus_zeros %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    mutate_if(is.numeric, round) %>%
    mutate_if(is.character, ~ ifelse(.x == "", NA_character_, .x)) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate observations to sample events
  # Calculate biomass_kgha_avg and biomass_kgha_by_trophic_group_avg

  biomass_kgha_by_trophic_group_cols <- sus %>%
    pull(biomass_kgha_by_trophic_group) %>%
    names()

  # In API, all are rounded to 2 decimal places - but just round to 0, because of some weird rounding issues in R
  sus_agg_for_se_comparison <- sus %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(sample_event_id, all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(biomass_kgha_by_trophic_group_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
              by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = abs(se - su) <= 1,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
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
  expect_equal(
    sus %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_by_benthic_category

  obs_agg <- obs %>%
    group_by(fake_sample_unit_id, benthic_category) %>%
    summarise(
      percent_cover_by_benthic_category = round(sum(length, na.rm = TRUE) * 100 / total_length, 2),
      .groups = "drop"
    ) %>%
    distinct() %>%
    pivot_wider(names_from = benthic_category, values_from = percent_cover_by_benthic_category)

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus %>%
    construct_fake_sample_unit_id() %>%
    unpack(percent_cover_by_benthic_category) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_by_benthic_category_avg and depth_avg

  percent_cover_by_benthic_category_cols <- sus %>%
    pull(percent_cover_by_benthic_category) %>%
    names()

  sus_agg_for_se_comparison <- sus %>%
    unpack(percent_cover_by_benthic_category) %>%
    select(sample_event_id, all_of(percent_cover_by_benthic_category_cols), depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(percent_cover_by_benthic_category_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
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
  expect_equal(
    sus %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_by_benthic_category
  # Do this by getting the length for each benthic category (sum of interval_size) divided by the total length (transect_length)

  obs_agg <- obs %>%
    group_by(fake_sample_unit_id, benthic_category) %>%
    summarise(
      percent_cover_by_benthic_category = round(sum(interval_size, na.rm = TRUE) * 100 / transect_length, 2),
      .groups = "drop"
    ) %>%
    distinct() %>%
    pivot_wider(names_from = benthic_category, values_from = percent_cover_by_benthic_category)

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus %>%
    construct_fake_sample_unit_id() %>%
    unpack(percent_cover_by_benthic_category) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to percent_cover_by_benthic_category_avg and depth_avg

  percent_cover_by_benthic_category_cols <- sus %>%
    pull(percent_cover_by_benthic_category) %>%
    names()

  sus_agg_for_se_comparison <- sus %>%
    unpack(percent_cover_by_benthic_category) %>%
    select(sample_event_id, all_of(percent_cover_by_benthic_category_cols), depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    unpack(percent_cover_by_benthic_category_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
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
  expect_equal(
    sus %>%
      nrow(),
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )
  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to score_avg

  obs_agg <- obs %>%
    group_by(fake_sample_unit_id) %>%
    summarise(
      score_avg = mean(score, na.rm = TRUE),
      .groups = "drop"
    )

  # Create "long" versions for comparing

  obs_agg_for_su_comparison <- obs_agg %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  sus_for_su_comparison <- sus %>%
    construct_fake_sample_unit_id() %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to score_avg_avg and depth_avg

  sus_agg_for_se_comparison <- sus %>%
    select(sample_event_id, score_avg_avg = score_avg, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su), 2),
      .groups = "drop"
    )

  ses_for_se_comparison <- ses %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se, 2))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})

# Bleaching -----

test_that("Bleaching sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

  obs <- mermaid_get_project_data(project_id, "bleaching", "observations")

  obs_colonies_bleached <- obs[["colonies_bleached"]] %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{quadrat_size}"))

  obs_percent_cover <- obs[["percent_cover"]] %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{quadrat_size}"))

  sus <- mermaid_get_project_data(project_id, "bleaching", "sampleunits")

  # Check first that there are the same number of fake SUs as real SUs
  obs_sample_units <- obs_colonies_bleached %>%
    dplyr::distinct(fake_sample_unit_id) %>%
    bind_rows(obs_percent_cover %>%
      distinct(fake_sample_unit_id)) %>%
    distinct(fake_sample_unit_id)

  expect_equal(
    sus %>%
      nrow(),
    obs_sample_units %>%
      nrow()
  )

  # Aggregate observations to sample units - no combining of fields like reef type, reef zone, etc etc
  # Aggregate colonies_bleached first - count_total, count_genera, percent_normal, percent_pale, percent_bleached

  obs_colonies_bleached_agg <- obs_colonies_bleached %>%
    group_by(fake_sample_unit_id) %>%
    summarise(count_bleached = sum(count_20, count_50, count_80, count_100, count_dead),
      count_total = sum(count_normal, count_pale, count_bleached, count_dead),
              count_genera = n_distinct(benthic_attribute),
              percent_normal = round(sum(count_normal) / count_total, 3)*100,
      percent_pale = round(sum(count_pale) / count_total, 3)*100,
      percent_bleached = round(sum(count_bleached) / count_total, 3)*100,
      .groups = "drop") %>%
    select(-count_bleached)

  # Aggregate percent_cover - quadrat_count, percent_hard_avg, percent_soft_avg, percent_algae_avg
  obs_percent_cover_agg <- obs_percent_cover %>%
    group_by(fake_sample_unit_id) %>%
    summarise(quadrat_count = n_distinct(quadrat_number),
              percent_hard_avg = round(mean(percent_hard), 1),
              percent_soft_avg = round(mean(percent_soft), 1),
              percent_algae_avg = round(mean(percent_algae), 1),
              .groups = "drop")

  # Create "long" versions for comparing

  obs_colonies_bleached_agg_for_su_comparison <- obs_colonies_bleached_agg %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  obs_percent_cover_agg_for_su_comparison <- obs_percent_cover_agg %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  obs_agg_for_su_comparison <- obs_colonies_bleached_agg_for_su_comparison %>%
    bind_rows(obs_percent_cover_agg_for_su_comparison)

  sus_for_su_comparison <- sus %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{quadrat_size}")) %>%
    select(fake_sample_unit_id, all_of(obs_agg_for_su_comparison[["name"]])) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- obs_agg_for_su_comparison %>%
    left_join(sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
  # Failing because SU values are being rounded for some reason!
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
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate SUs to SEs
  # depth_avg, quadrat_size_avg, count_total_avg, count_genera_avg, percent_normal_avg, percent_pale_avg, percent_bleached_avg, quadrat_count_avg, percent_hard_avg_avg, percent_soft_avg_avg, percent_algae_avg_avg

  sus_agg_for_se_comparison <- sus %>%
    select(sample_event_id, depth, quadrat_size, count_total, count_genera, percent_normal, percent_pale, percent_bleached, quadrat_count, percent_hard_avg, percent_soft_avg, percent_algae_avg) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    ) %>%
    mutate(name = paste0(name, "_avg"))

  ses_for_se_comparison <- ses %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- sus_agg_for_se_comparison %>%
    left_join(ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})
