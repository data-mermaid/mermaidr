test_that("mermaid_get_summary_sampleevents returns a tibble of all columns, plus unpacked protocols", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  summary_sampleevents <- mermaid_get_summary_sampleevents()
  non_protocol_cols <- legacy_columns[["summarysampleevents"]]

  expect_true(all(non_protocol_cols %in% names(summary_sampleevents)))

  summary_sampleevents_protocols <- summary_sampleevents %>%
    dplyr::select(-tidyselect::all_of(non_protocol_cols))

  expect_true(all(stringr::str_starts(names(summary_sampleevents_protocols), "beltfish|benthiclit|benthicpit|benthicpqt|habitatcomplexity|colonies_bleached|quadrat_benthic|macroinvertebrate")))
})
