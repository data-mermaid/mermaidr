test_that("mermaid_get_my_projects returns a list of projects that the authenticated user has access to", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_my_projects(limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["projects"]])
})

test_that("mermaid_get_my_projects by default only returns non-test projects, test projects too if flagged", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_my_projects(limit = 10)
  expect_true(all(output[["status"]] == "Open"))

  output <- mermaid_get_my_projects(limit = 10, include_test_projects = TRUE)
  expect_false(all(output[["status"]] == "Open"))
})

test_that("mermaid_get_my_projects returns `countries` and `tags` that are character columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_my_projects()
  expect_is(output[["countries"]], "character")
  expect_is(output[["tags"]], "character")
})
