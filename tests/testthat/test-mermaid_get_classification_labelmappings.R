test_that("mermaid_get_classification_labelmappings returns a lookup table of label mappings", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  output <- mermaid_get_classification_labelmappings()
  expect_named(output, classification_labelmappings_columns)
})

test_that("mermaid_get_classification_labelmappings filters table by `provider`", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  output <- mermaid_get_classification_labelmappings(provider = "CoralNet")
  expect_true(all(output[["provider"]] == "CoralNet"))

  output <- mermaid_get_classification_labelmappings(provider = "ReefCloud")
  if (nrow(output) > 0) {
    expect_true(all(output[["provider"]] == "ReefCloud"))
  } else {
    expect_named(output, classification_labelmappings_columns)
  }
})

test_that("mermaid_get_classification_labelmappings errors when provider is not one of the allowed ones", {

  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_get_classification_labelmappings("TEST"),
    "must be one of"
  )
})
