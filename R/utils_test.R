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
      dplyr::distinct(.data$fake_sample_unit_id) %>%
      nrow(),
    sus %>%
      nrow()
  )
}

# Test the number of fake SEs
test_n_fake_ses <- function(sus, ses) {
  testthat::expect_equal(
    ses %>%
      nrow(),
    sus %>%
      dplyr::distinct(.data$fake_sample_event_id) %>%
      nrow()
  )
}

# Compare (aggregated) values from SUs versus from SEs
test_sus_vs_ses_agg <- function(sus_agg, ses_agg) {
  sus_vs_ses_match <- sus_agg %>%
    dplyr::left_join(ses_agg,
      by = c("sample_event_id", "name")
    ) %>%
    dplyr::filter(!is.na(.data$se) | !is.na(.data$su)) %>%
    dplyr::mutate(
      match = .data$se == .data$su,
      match = dplyr::coalesce(.data$match, FALSE)
    )

  testthat::expect_true(all(sus_vs_ses_match[["match"]]))
}

# Compare (aggregated) values from observations versus from SUs
test_obs_vs_sus_agg <- function(obs_agg, sus_agg) {
  obs_vs_su_match <- obs_agg %>%
    dplyr::left_join(sus_agg,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    dplyr::filter(!is.na(.data$obs) | !is.na(.data$su)) %>%
    dplyr::mutate(
      match = .data$obs == .data$su,
      match = dplyr::coalesce(.data$match, FALSE)
    )

  testthat::expect_true(all(obs_vs_su_match[["match"]]))
}

# Fishbelt ----

## Obs to SUs

# Calculate biomass from observations, and convert to long format
calculate_obs_biomass_long <- function(obs) {
  obs_agg <- obs %>%
    dplyr::mutate(trophic_group = dplyr::coalesce(.data$trophic_group, "other")) %>%
    dplyr::group_by(.data$fake_sample_unit_id, .data$trophic_group) %>%
    dplyr::summarise(biomass_kgha_by_trophic_group = sum(.data$biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(biomass_kgha = sum(.data$biomass_kgha_by_trophic_group)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$trophic_group, values_from = .data$biomass_kgha_by_trophic_group)

  obs_agg %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

# Unpack biomass from SUs, and convert to long format
unpack_sus_biomass_long <- function(sus, obs_agg) {
  sus %>%
    tidyr::unpack(.data$biomass_kgha_by_trophic_group) %>%
    dplyr::select(.data$fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "su")
}

# SUs to SEs

# Calculate avg biomass from sus, and convert to long format
calculate_sus_biomass_avg_long <- function(sus) {
  biomass_kgha_by_trophic_group_cols <- sus %>%
    dplyr::pull(.data$biomass_kgha_by_trophic_group) %>%
    names()

  sus %>%
    tidyr::unpack(.data$biomass_kgha_by_trophic_group) %>%
    dplyr::select(.data$sample_event_id, dplyr::all_of(biomass_kgha_by_trophic_group_cols), biomass_kgha_avg = .data$biomass_kgha, depth_avg = .data$depth) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::group_by(.data$sample_event_id, .data$name) %>%
    dplyr::summarise(
      su = round(mean(.data$su)),
      .groups = "drop"
    )
}

# Unpack biomass from SEs, and convert to long format
unpack_ses_biomass_avg_long <- function(ses, sus_agg) {
  ses %>%
    tidyr::unpack(.data$biomass_kgha_by_trophic_group_avg) %>%
    dplyr::rename(sample_event_id = .data$id) %>%
    dplyr::select(.data$sample_event_id, sus_agg[["name"]]) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(.data$se)) %>%
    dplyr::mutate(se = round(.data$se))
}

# Benthic LIT ----

## Obs to SUs

calculate_lit_obs_percent_cover_long <- function(obs) {
  obs_agg <- obs %>%
    dplyr::group_by(.data$fake_sample_unit_id, .data$benthic_category) %>%
    dplyr::summarise(
      percent_cover_by_benthic_category = round(sum(.data$length, na.rm = TRUE) * 100 / .data$total_length, 2),
      .groups = "drop"
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = .data$benthic_category, values_from = .data$percent_cover_by_benthic_category)

  obs_agg %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

unpack_sus_percent_cover_long <- function(sus, obs_agg) {
  sus %>%
    construct_fake_sample_unit_id() %>%
    tidyr::unpack(.data$percent_cover_by_benthic_category) %>%
    dplyr::select(.data$fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "su")
}

# SUs to SEs

calculate_sus_percent_cover_avg_long <- function(sus) {
  percent_cover_by_benthic_category_cols <- sus %>%
    dplyr::pull(.data$percent_cover_by_benthic_category) %>%
    names()

  sus_agg_for_se_comparison <- sus %>%
    tidyr::unpack(.data$percent_cover_by_benthic_category) %>%
    dplyr::select(.data$sample_event_id, dplyr::all_of(percent_cover_by_benthic_category_cols), depth_avg = .data$depth) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::group_by(.data$sample_event_id, .data$name) %>%
    dplyr::summarise(
      su = round(mean(.data$su)),
      .groups = "drop"
    )
}

unpack_ses_percent_cover_avg_long <- function(ses, sus_agg) {
  ses %>%
    tidyr::unpack(.data$percent_cover_by_benthic_category_avg) %>%
    dplyr::rename(sample_event_id = .data$id) %>%
    dplyr::select(.data$sample_event_id, dplyr::all_of(sus_agg[["name"]])) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(.data$se)) %>%
    dplyr::mutate(se = round(.data$se))
}

# Benthic PIT ----

calculate_pit_obs_percent_cover_long <- function(obs) {
  obs %>%
    dplyr::group_by(.data$fake_sample_unit_id, .data$benthic_category) %>%
    dplyr::summarise(
      percent_cover_by_benthic_category = round(sum(.data$interval_size, na.rm = TRUE) * 100 / .data$transect_length, 2),
      .groups = "drop"
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = .data$benthic_category, values_from = .data$percent_cover_by_benthic_category) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

# Habitat Complexity ----

calculate_obs_score_long <- function(obs) {
  obs %>%
    dplyr::group_by(.data$fake_sample_unit_id) %>%
    dplyr::summarise(
      score_avg = mean(.data$score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

unpack_sus_score_long <- function(sus, obs_agg) {
  sus %>%
    construct_fake_sample_unit_id() %>%
    dplyr::select(.data$fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "su")
}

calculate_sus_score_avg_long <- function(sus) {
  sus %>%
    dplyr::select(.data$sample_event_id, score_avg_avg = .data$score_avg, depth_avg = .data$depth) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::group_by(.data$sample_event_id, .data$name) %>%
    dplyr::summarise(
      su = round(mean(.data$su), 2),
      .groups = "drop"
    )
}

unpack_ses_score_avg_long <- function(ses, sus_agg) {
  ses %>%
    dplyr::rename(sample_event_id = .data$id) %>%
    dplyr::select(.data$sample_event_id, dplyr::all_of(sus_agg[["name"]])) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(.data$se)) %>%
    dplyr::mutate(se = round(.data$se, 2))
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
    dplyr::group_by(.data$fake_sample_unit_id) %>%
    dplyr::summarise(
      count_bleached = sum(.data$count_20, .data$count_50, .data$count_80, .data$count_100, .data$count_dead),
      count_total = sum(.data$count_normal, .data$count_pale, .data$count_bleached),
      count_genera = dplyr::n_distinct(.data$benthic_attribute),
      percent_normal = round(sum(.data$count_normal) / .data$count_total, 3) * 100,
      percent_pale = round(sum(.data$count_pale) / .data$count_total, 3) * 100,
      percent_bleached = round(sum(.data$count_bleached) / .data$count_total, 3) * 100,
      .groups = "drop"
    ) %>%
    dplyr::select(-.data$count_bleached) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

calculate_obs_percent_cover_long <- function(obs_percent_cover) {
  obs_percent_cover %>%
    dplyr::group_by(.data$fake_sample_unit_id) %>%
    dplyr::summarise(
      quadrat_count = dplyr::n(),
      percent_hard_avg = round(mean(.data$percent_hard), 1),
      percent_soft_avg = round(mean(.data$percent_soft), 1),
      percent_algae_avg = round(mean(.data$percent_algae), 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
}

unpack_sus_bleaching_long <- function(sus, obs_agg) {
  sus %>%
    construct_bleaching_fake_sample_unit_id() %>%
    dplyr::select(.data$fake_sample_unit_id, dplyr::all_of(obs_agg[["name"]])) %>%
    dplyr::mutate_if(is.numeric, round) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "su")
}

calculate_sus_bleaching_long <- function(sus) {
  sus %>%
    dplyr::select(.data$sample_event_id, .data$depth, .data$quadrat_size, .data$count_total, .data$count_genera, .data$percent_normal, .data$percent_pale, .data$percent_bleached, .data$quadrat_count, .data$percent_hard_avg, .data$percent_soft_avg, .data$percent_algae_avg) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "su") %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::group_by(.data$sample_event_id, .data$name) %>%
    dplyr::summarise(
      su = round(mean(.data$su)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(name = paste0(.data$name, "_avg"))
}

unpack_sus_bleaching_avg_long <- function(ses, sus_agg) {
  ses %>%
    dplyr::rename(sample_event_id = .data$id) %>%
    dplyr::select(.data$sample_event_id, dplyr::all_of(sus_agg[["name"]])) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "se") %>%
    dplyr::filter(!is.na(.data$se)) %>%
    dplyr::mutate(se = round(.data$se))
}
