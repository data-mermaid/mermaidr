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

test_that("mermaid_GET allows the return of two endpoints as a named list", {
  skip_if_offline()
  output <- mermaid_GET(c("choices", "sites"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("choices", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))

  output <- mermaid_GET(c("sites", "sites"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))
})

test_that("mermaid_GET returns a single endpoint in a named list", {
  skip_if_offline()
  output <- mermaid_GET("sites", limit = 1)
  expect_is(output, "list")
  expect_named(output, "sites")
})
