test_that("mermaid_get_projects returns a list of projects depending on limit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_projects(limit = 1)
  expect_true(nrow(output) == 1)
})

test_that("mermaid_get_projects by default only returns non-test projects, test projects too if flagged", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_projects()
  expect_true(all(output[["status"]] == "Open"))

  output <- mermaid_get_projects(include_test_projects = TRUE)
  expect_false(all(output[["status"]] == "Open"))
})
