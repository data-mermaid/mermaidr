test_that("mermaid_get_reference returns a data frame", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_reference(reference = "fishfamilies", limit = 10)
  expect_true(nrow(output) == 10)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_reference allows multiple references", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  references <- c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes")
  output <- mermaid_get_reference(reference = references, limit = 2)
  expect_named(output, references)
  expect_true(all(sapply(output, nrow) == 2))
})

test_that("mermaid_get_reference errors if passed an unknown reference", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_get_reference("choices"), "must be one of")
})

test_that("mermaid_get_reference returns the regions with lookup values", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  fishfamilies <- mermaid_get_reference("fishfamilies")
  expect_true(any(stringr::str_detect(fishfamilies[["regions"]], "Eastern Indo-Pacific")))
})


test_that("mermaid_get_reference returns growth_form_life_histories as a list of DFs", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_get_reference("benthicattributes")
  expect_is(output[["growth_form_life_histories"]], "list")
  expect_true(purrr::map_lgl(output[["growth_form_life_histories"]], is.data.frame) %>% all())

  output <- mermaid_get_reference("benthicattributes", limit = 1)
  expect_is(output[["growth_form_life_histories"]], "list")
  expect_true(purrr::map_lgl(output[["growth_form_life_histories"]], is.data.frame) %>% all())
})
