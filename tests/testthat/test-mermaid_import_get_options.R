test_that("mermaid_import_get_options returns a list with the same names as mermaid_import_get_template, for a given method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"

  purrr::walk(c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity"), function(method) {
    option <- mermaid_import_get_options(project, method)
    template <- mermaid_import_get_template(method)
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

test_that("mermaid_import_get_options contains 'required' and 'help_text'", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(project, "habitatcomplexity")
  expect_true(all(c("required", "help_text") %in% names(options[["Visibility"]])))
})

test_that("mermaid_import_get_options with 'save' writes a sheet for every field, that contains required, help_text, and choices if not null", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(project, "habitatcomplexity", save = save_location)

  # Clean names to match excel sheet names
  names(options) <- purrr::map_chr(names(options), clean_excel_sheet_name)

  # Check sheet names
  sheet_names <- openxlsx::getSheetNames(save_location)
  expect_identical(names(options), sheet_names)

  # Read each sheet, check for "required" and "help_text" and "choices"
  purrr::walk(sheet_names, function(sheet) {
    contents <- openxlsx::read.xlsx(save_location, sheet = sheet, colNames = FALSE) %>%
      dplyr::mutate(id = dplyr::row_number())

    expect_identical("required", contents %>% dplyr::filter(id == 1) %>% dplyr::pull(X1))
    expect_identical(contents %>% dplyr::filter(id == 2) %>% dplyr::pull(X1) %>% as.logical(), options[[sheet]][["required"]])
    expect_identical("help_text", contents %>% dplyr::filter(id == 3) %>% dplyr::pull(X1))
    expect_identical(contents %>% dplyr::filter(id == 4) %>% dplyr::pull(X1), options[[sheet]][["help_text"]])

    options_choices <- options[[sheet]][["choices"]][["value"]]

    if (!is.null(options_choices)) {
      expect_identical(
        contents %>% dplyr::filter(id > 5) %>% dplyr::pull(X1),
        options_choices
      )
    }
  })
})

test_that("mermaid_import_get_options produces a message with where the file is saved", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  expect_message(mermaid_import_get_options(project, "habitatcomplexity", save = save_location), glue::glue("Import field options written to {save_location}"))
})

test_that("mermaid_import_get_options can take a project id or tibble", {
  expect_silent(mermaid_get_my_projects() %>%
    head(1) %>%
    mermaid_import_get_options("fishbelt"))

  expect_silent(mermaid_import_get_options("02e6915c-1c64-4d2c-bac0-326b560415a2", "benthicpit"))
})

test_that("mermaid_import_get_options only takes one project", {
  expect_error(
    mermaid_import_get_options(c("a", "b"), "benthiclit"),
    "only one project"
  )
})
