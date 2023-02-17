test_that("new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  new <- mermaid_get_project_data(p, method = "fishbelt", data = "observations")
  old <- mermaid_get_project_data_legacy(p, method = "fishbelt", data = "observations")

  # # Some conversion required - old has empty strings ("") while new has NA, difference in column types
  # old <- old %>% dplyr::mutate_all(as.character)
  # old <- old %>% dplyr::mutate_all(~ ifelse(.x == "", NA_character_, .x))
  # old <- old %>% dplyr::mutate_all(as.character)
  # new <- new %>% dplyr::mutate_all(as.character)

  expect_identical(old, new)

  new <- mermaid_get_project_data(p, method = "fishbelt", data = "sampleunits")
  old <- mermaid_get_project_data_legacy(p, method = "fishbelt", data = "sampleunits")

  # Some conversion required - old has empty strings ("") while new has NA, difference in column types
  old <- old %>% dplyr::mutate_all(as.character)
  old <- old %>% dplyr::mutate_all(~ ifelse(.x == "", NA_character_, .x))
  old <- old %>% dplyr::mutate_all(as.character)
  new <- new %>% dplyr::mutate_all(as.character)

  expect_identical(old, new)
})
