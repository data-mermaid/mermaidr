test_that("mermaid_list_projects returns a list of projects depending on limit", {
  skip_if_offline()
  output <- mermaid_list_projects(1)
  expect_true(nrow(output) == 1)
})

test_that("mermaid_list_projects by default only returns non-test projects, test projects too if flagged", {
  output <- mermaid_list_projects(limit = 500)
  expect_true(all(output[["status"]] == 90))

  output <- mermaid_list_projects(limit = 500, include_test_projects = TRUE)
  expect_false(all(output[["status"]] == 90))
})
