test_that("mermaid_get_reference returns a data frame with the correct names", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_reference(reference = "fishfamilies", limit = 10)
  expect_named(output, fishfamilies_columns)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_reference allows multiple references, and all are named correctly", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  references <- c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes")
  output <- mermaid_get_reference(reference = references, limit = 2)
  expect_named(output, references)
  expect_true(all(sapply(output, nrow) == 2))

  expect_named(output[["fishfamilies"]], fishfamilies_columns)
  expect_named(output[["fishgenera"]], fishgenera_columns)
  expect_named(output[["benthicattributes"]], benthicattributes_columns)
  expect_named(output[["fishspecies"]], c("id", "name", "species", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "regions", "created_on", "updated_on"))
})

test_that("mermaid_get_reference errors if passed an unknown reference", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_get_reference("choices"), "must be one of")
})

test_that("mermaid_get_reference returns the regions with lookup values", {
  fishfamilies <- mermaid_get_reference("fishfamilies")
  expect_true(any(stringr::str_detect(fishfamilies[["regions"]], "Eastern Indo-Pacific")))
})
