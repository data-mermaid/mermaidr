test_that("mermaid project sites return with the correct columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 2)
  output <- mermaid_get_project_sites(p, limit = 5)
  expect_named(output, c("project", project_sites_columns))
})
