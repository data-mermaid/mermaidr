test_that("mermaid_list_my_projects returns a list of projects that the authenticated user has access to", {
  skip_if_offline()
  output <- mermaid_list_my_projects(limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["projects"]])
})
