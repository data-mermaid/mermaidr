test_that("mermaid_import_project_data errors if data is not a data frame or path to a CSV file", {
  # Valid file, but not CSV
  expect_error(
    mermaid_import_project_data(system.file("extdata/mermaid_ingest.json", package = "mermaidr")),
    "path to a CSV"
  )

  # Invalid file
  expect_error(
    mermaid_import_project_data(tempfile()),
    "path to a CSV"
  )

  # List
  expect_error(
    mermaid_import_project_data(list()),
    "path to a CSV"
  )
})

test_that("mermaid_import_project_data errors if method doesn't match", {
  expect_error(
    mermaid_import_project_data(mtcars, method = "nope"),
    "must be one of"
  )
})
