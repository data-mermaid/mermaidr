test_that("multiplication works", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 2)
  output <- mermaid_get_project_sites(p, limit = 5)
  expect_named(output, c("project", project_sites_columns))
})
