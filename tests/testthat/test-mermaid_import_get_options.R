test_that("mermaid_import_get_options returns a list with the same names as mermaid_import_get_template, for a given method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(project, "all")
  templates <- mermaid_import_get_template("all")

  purrr::walk2(options, templates, function(option, template) {
    expect_identical(names(option), names(template))
  })
})

test_that("mermaid_import_get_options 'choices' field is not present if there are no choices", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(project, "habitatcomplexity")
  expect_null(options[["Sample unit notes"]][["choices"]])
})
