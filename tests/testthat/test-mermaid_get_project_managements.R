test_that("multiplication works", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 2)
  output <- mermaid_get_project_managements(p, limit = 5)
  expect_named(output, c("project", project_managements_columns))
})
