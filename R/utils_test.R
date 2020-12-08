# Testing utility functions

# General ----

# Construct a fake sample unit, which combines site, sample date, management, depth, transect number, and transect length to make an ID
construct_fake_sample_unit_id <- function(data) {
  data %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{transect_number}_{transect_length}"))
}

# Construct a fake sample unit, which combines site, date, and management to make an ID
construct_fake_sample_event_id <- function(data) {
  data %>%
    dplyr::mutate(fake_sample_event_id = glue::glue("{site}_{sample_date}_{management}"))
}

# Test that there are the same number of fake SUs as real SUs
test_n_fake_sus <- function(obs, sus) {
  testthat::expect_equal(
    obs %>%
      dplyr::distinct(fake_sample_unit_id) %>%
      nrow(),
    sus %>%
      nrow()
  )
}

# Test the number of fake SEs
test_n_fake_ses <- function(sus, ses) {
  expect_equal(
    ses %>%
      nrow(),
    sus %>%
      dplyr::distinct(fake_sample_event_id) %>%
      nrow()
  )
}

# Compare (aggregated) values from SUs versus from SEs
test_sus_vs_ses_agg <- function(sus_agg, ses_agg) {
  sus_vs_ses_match <- sus_agg %>%
    dplyr::left_join(ses_agg,
              by = c("sample_event_id", "name")
    ) %>%
    dplyr::filter(!is.na(se) | !is.na(su)) %>%
    dplyr::mutate(
      match = se == su,
      match = dplyr::coalesce(match, FALSE)
    )

  expect_true(all(sus_vs_ses_match[["match"]]))
}

# Compare (aggregated) values from observations versus from SUs
test_obs_vs_sus_agg <- function(obs_agg, sus_agg) {
  obs_vs_su_match <- obs_agg %>%
    dplyr::left_join(sus_agg,
              by = c("fake_sample_unit_id", "name")
    ) %>%
    dplyr::filter(!is.na(obs) | !is.na(su)) %>%
    dplyr::mutate(
      match = obs == su,
      match = dplyr::coalesce(match, FALSE)
    )

  expect_true(all(obs_vs_su_match[["match"]]))
}

# Fishbelt ----

## Obs to SUs

# Calculate biomass from observations, and convert to long format
calculate_obs_biomass_long <- function(obs) {
  obs_agg <- obs %>%
    dplyr::mutate(trophic_group = dplyr::coalesce(trophic_group, "other")) %>%
    dplyr::group_by(fake_sample_unit_id, trophic_group) %>%
    dplyr::summarise(biomass_kgha_by_trophic_group = sum(biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(biomass_kgha = sum(biomass_kgha_by_trophic_group)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = trophic_group, values_from = biomass_kgha_by_trophic_group)

  obs_agg %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

# Unpack biomass from SUs, and convert to long format
unpack_sus_biomass_long <- function(sus, obs_agg) {
  sus %>%
    tidyr::unpack(biomass_kgha_by_trophic_group) %>%
    dplyr::select(fake_sample_unit_id, tidyselect::all_of(obs_agg[["name"]])) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su")
}

# SUs to SEs

# Calculate avg biomass from sus, and convert to long format
calculate_sus_biomass_avg_long <- function(sus) {
  biomass_kgha_by_trophic_group_cols <- sus %>%
    dplyr::pull(biomass_kgha_by_trophic_group) %>%
    names()

  sus %>%
    tidyr::unpack(biomass_kgha_by_trophic_group) %>%
    dplyr::select(sample_event_id, tidyselect::all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = biomass_kgha, depth_avg = depth) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(su)) %>%
    dplyr::group_by(sample_event_id, name) %>%
    dplyr::summarise(
      su = round(mean(su)),
      .groups = "drop"
    )
}

# Unpack biomass from SEs, and convert to long format
unpack_ses_biomass_avg_long <- function(ses, sus_agg) {
  ses %>%
    tidyr::unpack(biomass_kgha_by_trophic_group_avg) %>%
    dplyr::rename(sample_event_id = id) %>%
    dplyr::select(sample_event_id, sus_agg[["name"]]) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(se)) %>%
    dplyr:: mutate(se = round(se))
}

# Benthic LIT ----

## Obs to SUs

calculate_lit_obs_percent_cover_long <- function(obs) {
  obs_agg <- obs %>%
    dplyr::group_by(fake_sample_unit_id, benthic_category) %>%
    dplyr::summarise(
      percent_cover_by_benthic_category = round(sum(length, na.rm = TRUE) * 100 / total_length, 2),
      .groups = "drop"
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = benthic_category, values_from = percent_cover_by_benthic_category)

  obs_agg %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

unpack_sus_percent_cover_long <- function(sus, obs_agg) {
  sus %>%
    construct_fake_sample_unit_id() %>%
    tidyr::unpack(percent_cover_by_benthic_category) %>%
    dplyr::select(fake_sample_unit_id, all_of(obs_agg[["name"]])) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su")
}

# SUs to SEs

calculate_sus_percent_cover_avg_long <- function(sus) {
  percent_cover_by_benthic_category_cols <- sus %>%
    dplyr::pull(percent_cover_by_benthic_category) %>%
    names()

  sus_agg_for_se_comparison <- sus %>%
    tidyr::unpack(percent_cover_by_benthic_category) %>%
    dplyr::select(sample_event_id, dplyr::all_of(percent_cover_by_benthic_category_cols), depth_avg = depth) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(su)) %>%
    dplyr::group_by(sample_event_id, name) %>%
    dplyr::summarise(
      su = round(mean(su)),
      .groups = "drop"
    )
}

unpack_ses_percent_cover_avg_long <- function(ses, sus_agg) {
  ses %>%
    tidyr::unpack(percent_cover_by_benthic_category_avg) %>%
    dplyr::rename(sample_event_id = id) %>%
    dplyr::select(sample_event_id, sus_agg[["name"]]) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(se)) %>%
    dplyr::mutate(se = round(se))
}

# Benthic PIT ----

calculate_pit_obs_percent_cover_long <- function(obs) {
  obs %>%
    dplyr::group_by(fake_sample_unit_id, benthic_category) %>%
    dplyr::summarise(
      percent_cover_by_benthic_category = round(sum(interval_size, na.rm = TRUE) * 100 / transect_length, 2),
      .groups = "drop"
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = benthic_category, values_from = percent_cover_by_benthic_category) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

# Habitat Complexity ----

calculate_obs_score_long <- function(obs) {
  obs %>%
    dplyr::group_by(fake_sample_unit_id) %>%
    dplyr::summarise(
      score_avg = mean(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

unpack_sus_score_long <- function(sus, obs_agg) {
  sus %>%
    construct_fake_sample_unit_id() %>%
    dplyr::select(fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su")
}

calculate_sus_score_avg_long <- function(sus) {
  sus %>%
    dplyr::select(sample_event_id, score_avg_avg = score_avg, depth_avg = depth) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(su)) %>%
    dplyr::group_by(sample_event_id, name) %>%
    dplyr::summarise(
      su = round(mean(su), 2),
      .groups = "drop"
    )
}

unpack_ses_score_avg_long <- function(ses, sus_agg) {
  ses %>%
    dplyr::rename(sample_event_id = id) %>%
    dplyr::select(sample_event_id, dplyr::all_of(sus_agg_for_se_comparison[["name"]])) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(se)) %>%
    dplyr::mutate(se = round(se, 2))
}

# Bleaching ----

# Construct a fake sample unit, which combines site, sample date, management, depth, and quadrat size to make an ID
construct_bleaching_fake_sample_unit_id <- function(data) {
  data %>%
    dplyr::mutate(fake_sample_unit_id = glue::glue("{site}_{sample_date}_{management}_{depth}_{quadrat_size}"))
}

# Obs to SUs

calculate_obs_colonies_long <- function(obs_colonies_bleached) {
  obs_colonies_bleached %>%
    dplyr::group_by(fake_sample_unit_id) %>%
    dplyr::summarise(count_bleached = sum(count_20, count_50, count_80, count_100, count_dead),
              count_total = sum(count_normal, count_pale, count_bleached, count_dead),
              count_genera = dplyr::n_distinct(benthic_attribute),
              percent_normal = round(sum(count_normal) / count_total, 3)*100,
              percent_pale = round(sum(count_pale) / count_total, 3)*100,
              percent_bleached = round(sum(count_bleached) / count_total, 3)*100,
              .groups = "drop") %>%
    dplyr::select(-count_bleached) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

calculate_obs_percent_cover_long <- function(obs_percent_cover) {
  obs_percent_cover %>%
    dplyr::group_by(fake_sample_unit_id) %>%
    dplyr::summarise(quadrat_count = dplyr::n_distinct(quadrat_number),
              percent_hard_avg = round(mean(percent_hard), 1),
              percent_soft_avg = round(mean(percent_soft), 1),
              percent_algae_avg = round(mean(percent_algae), 1),
              .groups = "drop") %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "obs")
}

unpack_sus_bleaching_long <- function(sus, obs_agg) {
  sus %>%
    construct_bleaching_fake_sample_unit_id() %>%
    dplyr::select(fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    tidyr::pivot_longer(-fake_sample_unit_id, values_to = "su")
}

calculate_sus_bleaching_long <- function(sus) {
  sus %>%
    dplyr::select(sample_event_id, depth, quadrat_size, count_total, count_genera, percent_normal, percent_pale, percent_bleached, quadrat_count, percent_hard_avg, percent_soft_avg, percent_algae_avg) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(su)) %>%
    dplyr::group_by(sample_event_id, name) %>%
    dplyr::summarise(
      su = round(mean(su)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(name = paste0(name, "_avg"))
}

unpack_sus_bleaching_avg_long <- function(ses, sus_agg) {
  ses %>%
    dplyr::rename(sample_event_id = id) %>%
    dplyr::select(sample_event_id, sus_agg_for_se_comparison[["name"]]) %>%
    tidyr::pivot_longer(-sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(se)) %>%
    dplyr::mutate(se = round(se))
}
