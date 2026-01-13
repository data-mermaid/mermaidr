test_that("blacklist method retains all columns from whitelist method", {
  expect_true(
    all(legacy_columns[["sites"]] %in%
      names(mermaid_get_sites()))
  )
})
