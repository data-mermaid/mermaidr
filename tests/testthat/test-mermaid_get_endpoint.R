test_that("mermaid_get_endpoint returns a data frame with the correct names", {
  skip_if_offline()
  output <- mermaid_get_endpoint(endpoint = "choices", limit = 10)
  expect_named(output, choices_columns)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_endpoint allows multiple endpoints", {
  skip_if_offline()
  output <- mermaid_get_endpoint(endpoint = c("choices", "projecttags", "fishsizes"), limit = 2)
  expect_named(output, c("choices", "projecttags", "fishsizes"))
  expect_true(all(sapply(output, nrow) == 2))
})

test_that("mermaid_get_endpoint errors if passed an unknown endpoint", {
  skip_if_offline()
  expect_error(mermaid_get_endpoint("benthicattributes"), "must be one of")
})
