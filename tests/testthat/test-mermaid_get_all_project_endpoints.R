mermaid_auth(token = here::here("tests", "testthat", ".httr-oauth"))

test_that("mermaid_get_all_project_endpoints returns a list of tibbles for all project endpoints", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  output <- mermaid_get_all_project_endpoints(test_project, limit = 1)
  expect_is(output, "list")
  expect_true(all_contain_value(lapply(output, class), "tbl_df"))
  expect_equal(names(output), mermaid_project_endpoint_columns)
})
