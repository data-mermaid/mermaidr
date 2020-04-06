test_that("mermaid_set_default_project sets environment variable, and mermaid_get_default_project retrieves it", {
  x <- "test"
  mermaid_set_default_project(x)
  expect_equal(mermaid_get_default_project(), x)
})

test_that("mermaid_set_default_project and get_default_project work with multiple projects", {
  skip_if_offline()
  p <- mermaid_list_my_projects()
  mermaid_set_default_project(p)
  expect_equal(mermaid_get_default_project(), p[["id"]])
  output <- mermaid_get_project_endpoint(endpoint = "managements", limit = 1)
  expect_is(output, "tbl_df")
  expect_true(all(output[["project_id"]] == p[["id"]]))
})
