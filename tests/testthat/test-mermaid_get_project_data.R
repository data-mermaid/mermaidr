library(dplyr)
library(tidyr)

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

# Function to construct a fake sample unit, which combines site, sample date, depth, transect number, and transect length to make an ID
construct_fake_sample_unit_id <- function(data) {
  data %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{depth}_{transect_number}_{transect_length}"))
}

# Function to construct a fake sample unit, which combines site and date to make an ID

construct_fake_sample_event_id <- function(data) {
  data %>%
    mutate(fake_sample_event_id = glue::glue("{site}_{sample_date}"))
}

# Fishbelt ----

# Vanilla fishbelt ----

project_id <- "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b"

fishbelt_vanilla_obs <- mermaid_get_project_data(project_id, "fishbelt", "observations")

fishbelt_vanilla_sus <- mermaid_get_project_data(project_id, "fishbelt", "sampleunits")

test_that("Vanilla fishbelt sample unit aggregation is the same as manually aggregating observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  fishbelt_vanilla_obs <- fishbelt_vanilla_obs %>%
    construct_fake_sample_unit_id()

  # Remove SUs with zero observations, since they don't appear in the observations endpoint and will mess up the comparisons

  fishbelt_vanilla_sus_minus_zeros <- fishbelt_vanilla_sus %>%
    dplyr::filter(biomass_kgha != 0) %>%
    construct_fake_sample_unit_id()

  # Check first that there are the same number of fake SUs as real SUs
  expect_equal(
    fishbelt_vanilla_sus_minus_zeros %>%
      nrow(),
    fishbelt_vanilla_obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow()
  )
  # Aggregate observations to sample units - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc
  # Just aggregate straight up to calculate biomass_kgha and biomass_kgha_by_trophic_group

  fishbelt_vanilla_obs_agg <- fishbelt_vanilla_obs %>%
    group_by(fake_sample_unit_id, trophic_group) %>%
    summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    ungroup() %>%
    pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  # Create "long" versions for comparing

  fishbelt_vanilla_obs_agg_for_su_comparison <- fishbelt_vanilla_obs_agg %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "obs")

  fishbelt_vanilla_sus_for_su_comparison <- fishbelt_vanilla_sus_minus_zeros %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(fake_sample_unit_id, all_of(fishbelt_vanilla_obs_agg_for_su_comparison[["name"]])) %>%
    mutate_if(is.numeric, round) %>%
    mutate_all(as.character) %>%
    pivot_longer(-fake_sample_unit_id, values_to = "su")

  # Check that values match

  obs_vs_su_match <- fishbelt_vanilla_obs_agg_for_su_comparison %>%
    left_join(fishbelt_vanilla_sus_for_su_comparison,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    filter(!is.na(obs) | !is.na(su)) %>%
    mutate(
      match = obs == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
})

fishbelt_vanilla_ses <- mermaid_get_project_data(project_id, "fishbelt", "sampleevents")

test_that("Vanilla fishbelt sample event aggregation is the same as manually aggregating sample units", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  fishbelt_vanilla_sus <- fishbelt_vanilla_sus %>%
    construct_fake_sample_event_id()

  # Check first that there are the same number of fake SEs as real SEs
  expect_equal(
    fishbelt_vanilla_ses %>%
      nrow(),
    fishbelt_vanilla_sus %>%
      distinct(fake_sample_event_id) %>%
      nrow()
  )

  expect_equal(
    fishbelt_vanilla_sus %>%
      distinct(fake_sample_event_id) %>%
      nrow(),
    fishbelt_vanilla_sus %>%
      distinct(sample_event_id) %>%
      nrow()
  )

  # Aggregate sample units to sample events - since this is vanilla fishbelt, there should be no combining of fields like reef type, reef zone, etc etc - but will want to check these in the other fishbelts!
  # Just aggregate straight up to calculate depth_avg, biomass_kgha_avg and biomass_kgha_by_trophic_group_avg, sample_unit_count

  biomass_kgha_by_trophic_group_cols <- fishbelt_vanilla_sus %>%
    pull(biomass_kgha_by_trophic_group) %>%
    names()

  # In API, all are rounded to 2 decimal places - but just round to 0, because of some weird rounding issues in R
  fishbelt_vanilla_sus_agg_for_se_comparison <- fishbelt_vanilla_sus %>%
    unpack(biomass_kgha_by_trophic_group) %>%
    select(sample_event_id, all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    pivot_longer(-sample_event_id, values_to = "su") %>%
    filter(!is.na(su)) %>%
    group_by(sample_event_id, name) %>%
    summarise(
      su = round(mean(su)),
      .groups = "drop"
    )

  fishbelt_vanilla_ses_for_se_comparison <- fishbelt_vanilla_ses %>%
    unpack(biomass_kgha_by_trophic_group_avg) %>%
    rename(sample_event_id = id) %>%
    select(sample_event_id, fishbelt_vanilla_sus_agg_for_se_comparison[["name"]]) %>%
    pivot_longer(-sample_event_id, values_to = "se") %>%
    filter(!is.na(se)) %>%
    mutate(se = round(se))

  # Check that values match

  sus_vs_ses_match <- fishbelt_vanilla_sus_agg_for_se_comparison %>%
    left_join(fishbelt_vanilla_ses_for_se_comparison,
      by = c("sample_event_id", "name")
    ) %>%
    filter(!is.na(se) | !is.na(su)) %>%
    mutate(
      match = se == su,
      match = coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
})
