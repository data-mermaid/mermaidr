test_that("mermaid_get_project_endpoint returns a tibble with specified names when passed a known endpoint.", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  test_project <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"
  expect_named(get_project_endpoint(test_project, "beltfishtransectmethods", limit = 1), project_other_endpoint_columns[["beltfishtransectmethods"]])
  expect_named(get_project_endpoint(test_project, "benthiclittransectmethods", limit = 1), project_other_endpoint_columns[["benthiclittransectmethods"]])
  expect_named(get_project_endpoint(test_project, "benthicpittransectmethods", limit = 1), project_other_endpoint_columns[["benthicpittransectmethods"]])
  expect_named(get_project_endpoint(test_project, "collectrecords", limit = 1), project_other_endpoint_columns[["collectrecords"]])
  expect_named(get_project_endpoint(test_project, "fishbelttransects", limit = 1), project_other_endpoint_columns[["fishbelttransects"]])
  expect_named(get_project_endpoint(test_project, "observers", limit = 1), project_other_endpoint_columns[["observers"]])
  expect_named(get_project_endpoint(test_project, "project_profiles", limit = 1), project_other_endpoint_columns[["project_profiles"]])
  expect_named(get_project_endpoint(test_project, "sampleevents", limit = 1), project_other_endpoint_columns[["sampleevents"]])
})
