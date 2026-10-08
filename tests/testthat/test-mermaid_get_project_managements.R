test_that("mermaid project managements returns the same cols as mermaid managements", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects(limit = 2)
  project_managements <- mermaid_get_project_managements(p, limit = 5)
  managements <- mermaid_get_managements(limit = 5)
  expect_equal(
    names(project_managements %>% dplyr::select(-project)) %>% sort(),
    names(managements) %>% sort()
  )
})

test_that("`project` is the first column", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects(limit = 1)
  project_managements <- mermaid_get_project_managements(p, limit = 1)
  expect_named(project_managements[,1], "project")
})

test_that("no mgmts = empty tibble", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "e343415f-d4e0-4a51-87cb-139ccd528b0b"
  expect_identical(mermaid_get_project_managements(p), dplyr::tibble())
  expect_identical(mermaid_get_project_managements(c(p, p)), dplyr::tibble())

  p <- lookup_project(p)
  expect_identical(mermaid_get_project_managements(p), dplyr::tibble())
  expect_identical(mermaid_get_project_managements(dplyr::bind_rows(p, p)), dplyr::tibble())
})

test_that("multiple projects, results are combined", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects(limit = 2)
  res <- mermaid_get_project_managements(p)
  expect_true(length(unique(res[["project"]])) == 2)
})
