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
