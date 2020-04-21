test_that("mermaid_get_managements returns a data frame of sites with the correct names", {
  skip_if_offline()
  output <- mermaid_get_managements(limit = 10)
  expect_named(output, managements_columns)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})
