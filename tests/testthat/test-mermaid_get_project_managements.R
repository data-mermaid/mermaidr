test_that("mermaid project managements returns the same cols as mermaid managements", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 2)
  project_managements <- mermaid_get_project_managements(p, limit = 5)
  managements <- mermaid_get_managements(limit = 5)
  expect_equal(
    names(project_managements %>% dplyr::select(-project)) %>% sort(),
    # TODO --> project_managements should also have rules, remove this once fixed
    names(managements %>% dplyr::select(-rules)) %>% sort()
  )
})
