test_that("mermaid_get_gfcr_report returns a list with set sheets/columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- c("75ef7a5a-c770-4ca6-b9f8-830cab74e425", "bacd3529-e0f4-40f4-a089-992c5bd5cc02")

  res <- mermaid_get_gfcr_report(p)

  expect_named(res, c(
    "F1", "F2", "F3", "F4", "F5", "F6", "F7", "BusinessesFinanceSolutions",
    "Investments", "Revenues", "ColumnDescriptions"
  ))

  expect_named(
    res[["F1"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Area (km2)", "Suggested Citation"
    )
  )

  expect_named(
    res[["F2"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Area (km2)"
    )
  )

  expect_named(
    res[["F3"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Value"
    )
  )

  expect_named(
    res[["F4"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Date Start",
      "Date End", "Sub-Indicator Name", "Value"
    )
  )

  expect_named(
    res[["F5"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Value"
    )
  )

  expect_named(
    res[["F6"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Value"
    )
  )

  expect_named(
    res[["F7"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Sub-Indicator Name",
      "Value"
    )
  )

  expect_named(
    res[["BusinessesFinanceSolutions"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Business / Finance Solution",
      "Sustainable Finance Mechanisms", "Sector", "Incubator Used",
      "GFCR Funded-Incubation", "Local Enterprise", "Gender Smart Investment"
    )
  )

  expect_named(
    res[["Investments"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Business / Finance Solution",
      "Sustainable Finance Mechanisms", "Sector", "Investment Source",
      "Investment Type", "Investment Amount"
    )
  )

  expect_named(
    res[["Revenues"]],
    c(
      "Project", "Title", "Reporting Date", "Data Type", "Business / Finance Solution",
      "Sustainable Finance Mechanisms", "Sector", "Revenue Type", "Sustainable Revenue Source",
      "Revenue Amount"
    )
  )

  expect_named(
    res[["ColumnDescriptions"]],
    c("FinanceColumnName", "Tabs", "Description", "Options", "Notes")
  )
})

test_that("mermaid_get_gfcr_report accepts one or multiple projects", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- c("75ef7a5a-c770-4ca6-b9f8-830cab74e425", "bacd3529-e0f4-40f4-a089-992c5bd5cc02")

  res_one_p <- mermaid_get_gfcr_report(p[[1]])

  res_multiple_p <- mermaid_get_gfcr_report(p)

  expect_named(res_one_p, names(res_multiple_p))
})


test_that("mermaid_get_gfcr_report with 'save' writes a file identical to what is saved", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  res <- mermaid_get_gfcr_report(project, save_location)

  expect_true(file.exists(save_location))

  file_sheets <- readxl::excel_sheets(save_location)
  res_file <- purrr::map(file_sheets, \(x) readxl::read_xlsx(save_location, sheet = x))
  names(res_file) <- file_sheets

  expect_identical(res, res_file)
})

test_that("mermaid_import_get_template_and_options produces a message with where the file is saved", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  save_location <- tempfile(fileext = ".xlsx")
  project <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  expect_message(mermaid_get_gfcr_report(project, save_location), glue::glue("GFCR report written to {save_location}"))
})
