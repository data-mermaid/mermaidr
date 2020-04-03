# mermaid_auth(token = here::here("tests", "testthat", ".httr-oauth"))

test_that("mermaid_get_all_project_endpoints returns a list of tibbles for all project endpoints", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  output <- mermaid_get_all_project_endpoints(test_project, limit = 1)
  expect_is(output, "list")
  expect_true(all_contain_value(lapply(output, class), "tbl_df"))
  expect_equal(names(output), names(mermaid_project_endpoint_columns))
})

test_that("mermaid_get_all_project_endpoints uses limit all throughout", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  output <- mermaid_get_all_project_endpoints(test_project, limit = 1)
  expect_true(all(sapply(output, nrow) <= 1))

  output <- mermaid_get_all_project_endpoints(test_project, limit = 2)
  expect_true(all(sapply(output, nrow) <= 2))

  output <- mermaid_get_all_project_endpoints(test_project)
  expect_true(length(unique(sapply(output, nrow))) > 2)
})

test_that("mermaid_get_all_project_endpoints can handle multiple projects (and so, all endpoints can handle it)", {
  library(dplyr)
  test_projects <- mermaid_list_my_projects(include_test_projects = TRUE) %>%
    filter(status == "Test")

  output <- mermaid_get_all_project_endpoints(test_projects, limit = 1)
  expect_true(all(sapply(output, nrow) <= 2))
  expect_true(any(sapply(output, nrow) == 2))

  output <- mermaid_get_all_project_endpoints(test_projects[["id"]], limit = 1)
  expect_true(all(sapply(output, nrow) <= 2))
  expect_true(any(sapply(output, nrow) == 2))
})
