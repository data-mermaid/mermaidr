test_that("check_excel_file errors if file is not xls/x and passes if it is", {
  expect_error(check_excel_file("test.csv"))
  expect_error(check_excel_file(".csv"))
  expect_error(check_excel_file("xlsx"))
  expect_error(check_excel_file(".xlsx"))
  expect_silent(check_excel_file("test.xlsx"))
  expect_silent(check_excel_file("test.xls"))
  expect_silent(check_excel_file("test.XLSX"))
  expect_silent(check_excel_file("/Users/sharla/Documents/Consulting/Wildlife Conservation Society/mermaidr/test.xlsx"))
})

test_that("mermaid_import_get_template_and_options returns a list with Template, and the remaining elements have the same names as the template, for a given method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"

  purrr::walk(c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"), function(method) {
    options_and_template <- mermaid_import_get_template_and_options(project, method)
    template <- options_and_template[["Template"]]
    options <- options_and_template[!names(options_and_template) == "Template"]
    expect_identical(names(options), names(template))
  })
})

test_that("mermaid_import_get_template_and_options 'choices' field is not present if there are no choices", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_template_and_options(project, "habitatcomplexity")
  expect_null(options[["Sample unit notes"]][["choices"]])
})

test_that("mermaid_import_get_template_and_options contains 'required' and 'help_text'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_template_and_options(project, "habitatcomplexity")
  expect_true(all(c("required", "help_text") %in% names(options[["Visibility"]])))
})

test_that("mermaid_import_get_template_and_options with 'save' writes a sheet for every field that has choices, and a metadata sheet that writes the name/required/help text for ALL fields", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_template_and_options(project, "habitatcomplexity", save = save_location)

  # Check template sheet
  template <- openxlsx::read.xlsx(save_location, "Template", sep.names = " ")
  expect_identical(names(options[["Template"]]), names(template))

  options <- options[!names(options) == "Template"]

  # Check metadata sheet
  metadata <- openxlsx::read.xlsx(save_location, "Metadata", sep.names = " ")
  expect_identical(metadata[["Column Name"]], names(options))
  options_transposed <- options %>% purrr::transpose()
  expect_identical(metadata[["Required"]], ifelse(options_transposed[["required"]], "Yes", "No"))
  expect_identical(metadata[["Description"]], unlist(options_transposed[["help_text"]]) %>% unname())

  # Clean names to match excel sheet names
  names(options) <- purrr::map_chr(names(options), clean_excel_sheet_name)

  # Get cols that have options
  cols_with_options <- options %>%
    purrr::keep(~ "choices" %in% names(.x)) %>%
    names()

  # Check sheet names
  sheet_names <- openxlsx::getSheetNames(save_location)
  expect_identical(c("Template", "Metadata", cols_with_options), sheet_names)

  # Read each sheet, check choices
  purrr::walk(cols_with_options, function(sheet) {
    contents <- openxlsx::read.xlsx(save_location, sheet = sheet, colNames = FALSE)

    expect_identical(contents %>% dplyr::pull(X1), options[[sheet]][["choices"]][["value"]])
  })
})

test_that("mermaid_import_get_template_and_options produces a message with where the file is saved", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  expect_message(mermaid_import_get_template_and_options(project, "habitatcomplexity", save = save_location), glue::glue("Import template and field options written to {save_location}"))
})

test_that("mermaid_import_get_template_and_options can take a project id or tibble", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_silent(mermaid_get_my_projects() %>%
    head(1) %>%
    mermaid_import_get_template_and_options("fishbelt"))

  expect_silent(mermaid_import_get_template_and_options("02e6915c-1c64-4d2c-bac0-326b560415a2", "benthicpit"))
})

test_that("mermaid_import_get_template_and_options only takes one project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_import_get_template_and_options(c("a", "b"), "benthiclit"),
    "only one project"
  )
})
