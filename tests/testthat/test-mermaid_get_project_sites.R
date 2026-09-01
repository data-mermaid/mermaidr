test_that("mermaid project sites returns the same cols as mermaid sites", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 2)
  project_sites <- mermaid_get_project_sites(p, limit = 5)
  sites <- mermaid_get_sites(limit = 5)
  expect_equal(
    names(project_sites) %>% sort(),
    names(sites) %>% sort()
  )
})

test_that("covariates produces a warning", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_message(
    mermaid_get_project_sites("170e7182-700a-4814-8f1e-45ee1caf3b44", covariates = TRUE),
    "deprecated"
  )
})

test_that("`project` is the first column", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects(limit = 1)
  project_sites <- mermaid_get_project_sites(p, limit = 1)
  expect_named(project_sites[,1], "project")
})
