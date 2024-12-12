test_that("mermaid_get_project_classification_images returns expected columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "df2bb123-5bfc-4f3f-aa88-bb102d3b2b35"
  e <- "classification/images"

  expected_cols <- classification_images_endpoint_columns[[e]]
  expected_cols_expanded <- classification_images_endpoint_columns_expanded[[e]]

  res <- mermaid_get_project_classification_images(p, limit = 1)
  expect_true(all(expected_cols[!expected_cols %in% expected_cols_expanded] %in% names(res)))
  expect_true(purrr::map_lgl(
    expected_cols_expanded,
    \(x) {
      any(stringr::str_starts(names(res), x))
    }
  ) %>% all())
})

test_that("mermaid_get_project_classification_images does not return columns in `exclude`", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "df2bb123-5bfc-4f3f-aa88-bb102d3b2b35"
  e <- "classification/images"

  exclude_cols <- c("data", "points", "created_by", "updated_by", "created_on", "updated_on", "original_image_width", "original_image_height", "location", "comments", "image", "photo_timestamp")

  res <- mermaid_get_project_classification_images(p, exclude = exclude_cols, limit = 1)
  expect_true(!any(exclude_cols %in% names(res)))

  expected_cols <- classification_images_endpoint_columns[[e]]
  expected_cols_expanded <- classification_images_endpoint_columns_expanded[[e]]
  expected_cols <- expected_cols[!expected_cols %in% expected_cols_expanded]
  expected_cols <- expected_cols[!expected_cols %in% exclude_cols]

  expect_true(all(expected_cols %in% names(res)))
})
