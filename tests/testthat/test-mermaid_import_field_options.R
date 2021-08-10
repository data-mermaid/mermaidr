test_that("mermaid_import_field_options returns options for each field listed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  field_options <- c("width", "fishsizebin", "reefslope", "visibility", "current", "relativedepth", "tide", "fishname", "benthicattribute", "growthform", "habitatcomplexityscore")
  res <- purrr::map(field_options, mermaid_import_field_options)
  names(res) <- field_options

  purrr::walk(res, expect_s3_class, "tbl_df")
})

test_that("mermai_import_field_options errors if the field is not one listed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(mermaid_import_field_options("growthforms"), "must be one of")
})
