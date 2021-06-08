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

test_that("mermaid_import_project_data errors with an invalid project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(
    mermaid_import_project_data(dplyr::tibble(), "test", method = "fishbelt"),
    "is not a valid project_id"
  )
})

test_that("mermaid_import_project_data errors if you are not part of the project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_import_project_data(dplyr::tibble(), "00673bdf-b838-4c2e-a305-86c99c378ec5", "fishbelt"),
    "You are not part of this project"
  )
})

test_that("mermaid_import_project_data errors if the data does not contain the correct columns for the method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_import_project_data(dplyr::tibble(x = 1), "02e6915c-1c64-4d2c-bac0-326b560415a2", "benthiclit"),
    "Missing required fields"
  )
})

test_that("mermaid_import_project_data warns and returns a df if there are data issues", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(`Site *` = c("Ada01", "Ada01"), `Management *` = c(
    "Adavaci_open",
    "Adavaci_open"
  ), `Sample date: Year *` = c(2017, 2017), `Sample date: Month *` = c(
    5,
    5
  ), `Sample date: Day *` = c(15, 15), `Sample time` = structure(c(
    43200,
    43200
  ), class = c("hms", "difftime"), units = "secs"), `Depth *` = c(
    8,
    8
  ), `Transect number *` = c(1, 1), `Transect label` = c(NA, NA), `Transect length surveyed *` = c(50, 50), `Width *` = c(
    5,
    5
  ), `Fish size bin *` = c(5, 5), `Reef slope` = c(NA, NA), Visibility = c(
    NA,
    NA
  ), Current = c(NA, NA), `Relative depth` = c("Deep", "Deep"), Tide = c("falling", "falling"), Notes = c(NA, NA), `Observer emails *` = c(
    "wnaisilisili@wcs.org",
    "wnaisilisili@wcs.org"
  ), `Fish name *` = c(
    "chaetodon auriga",
    "heniochus varius"
  ), `Size *` = c(7.5, 7.5), `Count *` = c(
    4,
    2
  )), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))

  expect_warning(
    mermaid_import_project_data(df, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "fishbelt"),
    "Problems"
  )

  df <- suppressWarnings(mermaid_import_project_data(df, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "fishbelt"))
  expect_s3_class(df, "data.frame")
})

test_that("mermaid_import_project_data with no validation errors and dryrun = TRUE does not actually write to Collect", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(
    `Site *` = "1201", `Management *` = "Fake Management Organization",
    `Sample date: Year *` = 2017, `Sample date: Month *` = 5,
    `Sample date: Day *` = 15, `Sample time` = structure(43200, class = c(
      "hms",
      "difftime"
    ), units = "secs"), `Depth *` = 8, `Transect number *` = 1,
    `Transect label` = NA, `Transect length surveyed *` = 50,
    `Width *` = "5m", `Fish size bin *` = 5, `Reef slope` = NA,
    Visibility = NA, Current = NA, `Relative depth` = "Deep",
    Tide = "falling", Notes = NA, `Observer emails *` = "sharla.gelfand@gmail.com",
    `Fish name *` = "chaetodon auriga", `Size *` = 7.5, `Count *` = 4
  ), row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame"))

  project_id <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"
  collect_records_before <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_message(mermaid_import_project_data(df, project_id, "fishbelt"), "Records successfully checked! To import, please run the function again")
  collect_records_after <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_identical(collect_records_before, collect_records_after)
})

test_that("mermaid_import_project_data with no validation errors and dryrun = FALSE *does* write to Collect", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(
    `Site *` = "1201", `Management *` = "Fake Management Organization",
    `Sample date: Year *` = 2017, `Sample date: Month *` = 5,
    `Sample date: Day *` = 15, `Sample time` = structure(43200, class = c(
      "hms",
      "difftime"
    ), units = "secs"), `Depth *` = 8, `Transect number *` = 1,
    `Transect label` = NA, `Transect length surveyed *` = 50,
    `Width *` = "5m", `Fish size bin *` = 5, `Reef slope` = NA,
    Visibility = NA, Current = NA, `Relative depth` = "Deep",
    Tide = "falling", Notes = NA, `Observer emails *` = "sharla.gelfand@gmail.com",
    `Fish name *` = "chaetodon auriga", `Size *` = 7.5, `Count *` = 4
  ), row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame"))

  project_id <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"
  collect_records_before <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_message(mermaid_import_project_data(df, project_id, "fishbelt", dryrun = FALSE), "Records successfully imported! Please review in Collect")
  collect_records_after <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_true(nrow(collect_records_after) == nrow(collect_records_before) + nrow(df))

  df_in_file <- tempfile(fileext = ".csv")
  readr::write_csv(df, df_in_file)
  collect_records_before <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_message(mermaid_import_project_data(df_in_file, project_id, "fishbelt", dryrun = FALSE), "Records successfully imported! Please review in Collect")
  collect_records_after <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_true(nrow(collect_records_after) == nrow(collect_records_before) + nrow(df))
})

test_that("mermaid_import_project_data errors if both dryrun and clearexisting are TRUE", {
  expect_error(mermaid_import_project_data(dplyr::tibble(x = 1), "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", method = "fishbelt", dryrun = TRUE, clearexisting = TRUE), "Please double check which option you would like to set")
})
