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
    dplyr::full_join(ses_agg,
      by = c("sample_event_id", "name")
    ) %>%
    dplyr::filter(!is.na(.data$se) | !is.na(.data$su)) %>%
    dplyr::mutate(dplyr::across(c(.data$se, .data$su), as.numeric))


  testthat::expect_true(all(sus_vs_ses_match[["se"]] -  sus_vs_ses_match[["su"]] < 1))
}

# Compare (aggregated) values from observations versus from SUs
test_obs_vs_sus_agg <- function(obs_agg, sus_agg) {
  obs_vs_su_match <- obs_agg %>%
    dplyr::full_join(sus_agg,
      by = c("fake_sample_unit_id", "name")
    ) %>%
    dplyr::filter(!is.na(.data$obs) | !is.na(.data$su)) %>%
    dplyr::mutate_if(is.double, as.numeric) %>%
    dplyr::mutate_if(is.numeric, dplyr::coalesce, 0)

  if(any(is.na(suppressWarnings(as.numeric(obs_vs_su_match[["su"]]))))) {
    testthat::expect_true(all(obs_vs_su_match[["obs"]] == obs_vs_su_match[["su"]]))
  } else {
    testthat::expect_true(all(obs_vs_su_match[["obs"]] - obs_vs_su_match[["su"]] < 1))
  }
}

# Fishbelt ----

## Obs to SUs

# Calculate biomass from observations, and convert to long format
calculate_obs_biomass_long <- function(obs, aggregate_cols = c("trophic_group", "fish_family")) {
  aggregate_by_col <- function(col) {
    col <- dplyr::sym(col)

    obs %>%
      dplyr::mutate(
        {{ col }} := dplyr::coalesce({{ col }}, glue::glue("{rlang::as_string(col)}-other")),
        {{ col }} := stringr::str_to_lower({{ col }}),
        {{ col }} := stringr::str_replace_all({{ col }}, "-", "_")
      ) %>%
      dplyr::group_by(.data$fake_sample_unit_id, {{ col }}) %>%
      dplyr::summarise(biomass_by_col = sum(.data$biomass_kgha, na.rm = TRUE), .groups = "drop_last") %>%
      dplyr::mutate(biomass_kgha = sum(.data$biomass_by_col)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = {{ col }}, values_from = .data$biomass_by_col) %>%
      tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs")
  }

  aggregate_cols %>%
    purrr::map(aggregate_by_col) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(obs)) %>%
    dplyr::distinct()
}

# Aggregate biomass from SUs, and convert to long format
aggregate_sus_biomass_long <- function(sus, aggregate_cols = c("trophic_group", "fish_family")) {
  aggregate_by_col <- function(col) {
    sus %>%
      dplyr::select(.data$fake_sample_unit_id, .data$biomass_kgha, dplyr::contains(col)) %>%
      tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "su", names_prefix = paste0("biomass_kgha_", col, "_")) %>%
      dplyr::mutate(
      name = dplyr::case_when(
        stringr::str_detect(.data$name, "other") ~ glue::glue("{col}_other"),
        TRUE ~ .data$name
      ))
  }

  aggregate_cols %>%
    purrr::map(aggregate_by_col) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::distinct()
}

# SUs to SEs

# Calculate avg biomass from sus, and convert to long format
calculate_sus_biomass_avg_long <- function(sus, aggregate_cols = c("trophic_group", "fish_family")) {
  avg_by_col <- function(col) {
    sus %>%
      dplyr::select(.data$sample_event_id, dplyr::starts_with(col), biomass_kgha_avg = .data$biomass_kgha, depth_avg = .data$depth) %>%
      tidyr::pivot_longer(-.data$sample_event_id, values_to = "su") %>%
      dplyr::filter(!is.na(.data$su)) %>%
      dplyr::group_by(.data$sample_event_id, .data$name) %>%
      dplyr::summarise(su = mean(.data$su, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        name = stringr::str_remove_all(.data$name, paste0(col, "_")),
        name = stringr::str_remove_all(.data$name, "_biomass_kgha_avg|_biomass_kgha"),
        name = dplyr::case_when(
          stringr::str_detect(.data$name, "other") ~ glue::glue("{col}_other"),
          TRUE ~ .data$name
        ))
  }

  aggregate_cols %>%
    purrr::map(avg_by_col) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
}

# Unpack biomass from SEs, and convert to long format
aggregate_ses_biomass_avg_long <- function(ses, aggregate_cols = c("trophic_group", "fish_family")) {
  aggregate_by_col <- function(col) {
    ses %>%
      dplyr::rename(sample_event_id = .data$id) %>%
      dplyr::select(.data$sample_event_id, dplyr::starts_with(col), .data$depth_avg, .data$biomass_kgha_avg) %>%
      tidyr::pivot_longer(-.data$sample_event_id, values_to = "se") %>%
      dplyr::filter(!is.na(.data$se)) %>%
      dplyr::mutate(
        name = stringr::str_remove_all(.data$name, paste0(col, "_")),
        name = stringr::str_remove_all(.data$name, "_biomass_kgha_avg|_biomass_kgha"),
        name = dplyr::case_when(
          stringr::str_detect(.data$name, "other") ~ glue::glue("{col}_other"),
          TRUE ~ .data$name
        )
      )
  }

  aggregate_cols %>%
    purrr::map(aggregate_by_col) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()
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
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs") %>%
    dplyr::mutate(
      name = stringr::str_to_lower(.data$name),
      name = stringr::str_replace_all(.data$name, "-| ", "_")
    ) %>%
    dplyr::filter(!is.na(.data$obs))
}

aggregate_sus_percent_cover_long <- function(sus) {
  sus %>%
    construct_fake_sample_unit_id() %>%
    dplyr::select(.data$fake_sample_unit_id, dplyr::starts_with("percent_cover_benthic_category")) %>%
    tidyr::pivot_longer(-.data$fake_sample_unit_id,
      values_to = "su",
      names_prefix = "percent_cover_benthic_category_"
    ) %>%
    dplyr::filter(!is.na(.data$su))
}

# SUs to SEs

calculate_sus_percent_cover_avg_long <- function(sus) {
  sus_agg_for_se_comparison <- sus %>%
    dplyr::select(.data$sample_event_id, dplyr::starts_with("percent_cover_benthic_category"), depth_avg = .data$depth) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "su", names_prefix = "percent_cover_benthic_category_") %>%
    dplyr::filter(!is.na(.data$su)) %>%
    dplyr::group_by(.data$sample_event_id, .data$name) %>%
    dplyr::summarise(
      su = mean(.data$su),
      .groups = "drop"
    )
}

aggregate_ses_percent_cover_avg_long <- function(ses, sus_agg) {
  ses %>%
    dplyr::select(sample_event_id = .data$id, dplyr::starts_with("percent_cover_benthic_category_avg"), .data$depth_avg) %>%
    tidyr::pivot_longer(-.data$sample_event_id, values_to = "se", names_prefix = "percent_cover_benthic_category_avg_") %>%
    dplyr::filter(!is.na(.data$se))
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
    tidyr::pivot_longer(-.data$fake_sample_unit_id, values_to = "obs") %>%
    dplyr::mutate(
      name = stringr::str_to_lower(.data$name),
      name = stringr::str_replace_all(.data$name, "-| ", "_")
    ) %>%
    dplyr::filter(!is.na(.data$obs))
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
