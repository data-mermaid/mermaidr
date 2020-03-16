test_that("mermaid_set_default_project sets environment variable, and mermaid_get_default_project retrieves it", {
  x <- "test"
  mermaid_set_default_project(x)
  expect_equal(mermaid_get_default_project(), x)
})
