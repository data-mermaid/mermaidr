test_that("mermaid_GET throws error when endpoint does not exist", {
  skip_if_offline()
  expect_error(mermaid_GET("slgkjgs"), regexp = "Mermaid API request failed: (404) Not Found", fixed = TRUE)
})

test_that("mermaid_GET returns projects using showall query parameter", {
  skip_if_offline()
  expect_equal(nrow(mermaid_GET("projects", limit = 1)), 1)
})

test_that("mermaid_GET returns a tibble with column data of nested tibbles if the endpoint is choices", {
  skip_if_offline()
  output <- mermaid_GET("choices")
  expect_s3_class(output, "tbl_df")
  expect_s3_class(output[["data"]][[1]], "tbl_df")
})

test_that("mermaid_GET does value lookups if endpoint is sites", {
  skip_if_offline()
  output <- mermaid_GET("sites", limit = 5)
  expect_true(all(c("country_id", "country_name") %in% names(output)))
})
