test_that("mermaid_get_sites returns a data frame of sites", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_sites(limit = 10)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_sites fails without authentication", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_get_sites(token = NULL), "Unauthorized")
})
