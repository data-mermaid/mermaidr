test_that("mermaid_clean_columns unpacks df cols, does not add prefix by default", {
  df <- dplyr::tibble(
    x = 1,
    y = dplyr::tibble(a = 1, b = 2)
  )

  output <- mermaid_clean_columns(df)
  expect_named(output, c("x", "a", "b"))
})

test_that("mermaid_clean_columns adds prefix if asked to", {
  df <- dplyr::tibble(
    x = 1,
    y = dplyr::tibble(a = 1, b = 2)
  )
  expect_named(mermaid_clean_columns(df, append_column_prefix = TRUE), c("x", "y_a", "y_b"))
})

test_that("mermaid_clean_columns cleans names by default, doesn't if set not to", {
  df <- dplyr::tibble(
    x = 1,
    y = dplyr::tibble(`Type One` = 1, `type-2` = 2)
  )
  expect_named(mermaid_clean_columns(df), c("x", "type_one", "type_2"))
  expect_named(mermaid_clean_columns(df, clean_names = FALSE), c("x", "Type One", "type-2"))
})

test_that("mermaid_clean_columns renames any 'other' columns to whatever they are by", {
  df <- dplyr::tibble(
    x = 1,
    measurement_by_trophic_group = dplyr::tibble(other = 1),
    measurement_by_cat = dplyr::tibble(other = 1)
  )

  expect_named(mermaid_clean_columns(df), c("x", "other_trophic_group", "other_cat"))
})
