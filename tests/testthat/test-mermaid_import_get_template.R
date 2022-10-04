test_that("mermaid_import_get_template returns a single, 0 row named tibble if there is one (not 'all') method passed", {
  template <- mermaid_import_get_template("fishbelt")
  expect_s3_class(template, "tbl_df")
  expect_true(nrow(template) == 0)
  expect_named(template)
})

test_that("mermaid_import_get_template returns a list of 0 row named tibbles if multiple methods are passed", {
  methods <- c("fishbelt", "benthicpit")
  template <- mermaid_import_get_template(methods)
  expect_named(template, methods)
  purrr::walk(template, ~ expect_true(nrow(.x) == 0))
  purrr::walk(template, expect_named)
})

test_that("mermaid_import_get_template returns a list of 0 row named tibbles, one for every method, if 'all' is passed", {
  template <- mermaid_import_get_template("all")
  expect_named(template, c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity"))
  purrr::walk(template, ~ expect_true(nrow(.x) == 0))
  purrr::walk(template, expect_named)
})
