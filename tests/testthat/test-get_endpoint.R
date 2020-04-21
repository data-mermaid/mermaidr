test_that("mermaid_get_endpoint throws an error when an unexpected endpoint is passed", {
  expect_error(get_endpoint("ldkgjdgk"), "be one of")
})

test_that("mermaid_get_endpoint returns a tibble when passed a known endpoint.", {
  skip_if_offline()
  expect_is(get_endpoint("sites", limit = 1), "tbl_df")
})

test_that("mermaid_get_endpoint can return for multiple endpoints", {
  output <- get_endpoint(c("sites", "managements"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "managements"))
})
