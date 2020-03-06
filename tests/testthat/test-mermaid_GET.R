test_that("mermaid_GET throws error when endpoint does not exist", {
  skip_if_offline()
  expect_error(mermaid_GET("slgkjgs"), regexp = "Mermaid API request failed: (404) Not Found", fixed = TRUE)
})
