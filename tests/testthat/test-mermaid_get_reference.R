test_that("mermaid_get_reference returns a data frame with the correct names", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  output <- mermaid_get_reference(reference = "fishfamilies", limit = 10)
  expect_named(output, fishfamilies_columns)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_reference allows multiple references", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  output <- mermaid_get_reference(reference = c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"), limit = 2)
  expect_named(output, c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"))
  expect_true(all(sapply(output, nrow) == 2))
})

test_that("mermaid_get_reference errors if passed an unknown reference", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  expect_error(mermaid_get_reference("choices"), "must be one of")
})
