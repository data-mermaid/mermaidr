test_that("mermaid_import_project_data errors if data is not a data frame or path to a CSV file", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects() %>%
    head(1)

  # Valid file, but not CSV
  expect_error(
    mermaid_import_project_data(system.file("extdata/mermaid_ingest.json", package = "mermaidr"), p),
    "path to a CSV"
  )

  # Invalid file
  expect_error(
    mermaid_import_project_data(tempfile(), p),
    "path to a CSV"
  )

  # List
  expect_error(
    mermaid_import_project_data(list(), p),
    "path to a CSV"
  )
})

test_that("mermaid_import_project_data errors if method doesn't match", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_my_projects() %>%
    head(1)

  expect_error(
    mermaid_import_project_data(mtcars, p, method = "nope"),
    "must be one of"
  )
})

test_that("mermaid_import_project_data errors with an invalid project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(
    mermaid_import_project_data(dplyr::tibble(), "test", method = "fishbelt"),
    "is not a valid project ID"
  )
})

test_that("mermaid_import_project_data errors if you are not part of the project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_import_project_data(dplyr::tibble(), "00673bdf-b838-4c2e-a305-86c99c378ec5", "fishbelt"),
    "You do not have permission to perform this action"
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

test_that("mermaid_import_project_data renames $row_number to row_number and starts on the first row of data, not the header", {
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

  df <- suppressWarnings(mermaid_import_project_data(df, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "fishbelt"))
  expect_named(df[1], "row_number")
  expect_identical(df[[1]], c(1, 2))
})

test_that("mermaid_import_project_data with no validation errors and dryrun = TRUE does not actually write to Collect", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(
    `Site *` = "1201", `Management *` = "Fake Management Organization",
    `Sample date: Year *` = 2017, `Sample date: Month *` = 5,
    `Sample date: Day *` = 15, `Sample time` = "10:01", `Depth *` = 8, `Transect number *` = 1,
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
    `Sample date: Day *` = 15, `Sample time` = "10:01", `Depth *` = 8, `Transect number *` = 1,
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
  write.csv(df, df_in_file, row.names = FALSE)
  collect_records_before <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_message(mermaid_import_project_data(df_in_file, project_id, "fishbelt", dryrun = FALSE), "Records successfully imported! Please review in Collect")
  collect_records_after <- mermaid_get_project_endpoint(project_id, "collectrecords")
  expect_true(nrow(collect_records_after) == nrow(collect_records_before) + nrow(df))
})

test_that("mermaid_import_project_data errors if both dryrun and clearexisting are TRUE", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(mermaid_import_project_data(dplyr::tibble(x = 1), "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", method = "fishbelt", dryrun = TRUE, clearexisting = TRUE), "Please double check which option you would like to set")
})

test_that("mermaid_import_project_data coerces NA Sample time to empty string which successfully uploads", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(
    `Site *` = c("Batu Mandi", "Batu Mandi"),
    `Management *` = c(
      "Luar Kawasan",
      "Luar Kawasan"
    ), `Sample date: Year *` = c(2009, 2016),
    `Sample date: Month *` = c(12, 4), `Sample date: Day *` = c(5, 18),
    `Sample time` = c("01:00", "02:33"), `Depth *` = c(29, 6),
    `Quadrat size *` = c(2, 2),
    `Observer emails *` = c("sharla.gelfand@gmail.com", "sharla.gelfand@gmail.com"),
    `Quadrat number` = c(1, 1),
    `Hard coral % cover` = c(5, 5),
    `Macroalgae coral % cover` = c(5, 5),
    `Soft coral % cover` = c(5, 5)
  ), row.names = c(NA, -2L), class = c(
    "tbl_df", "tbl",
    "data.frame"
  ))

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"

  expect_message(mermaid_import_project_data(df, p, method = "bleaching"), "successfully")

  temp <- tempfile(fileext = ".csv")

  utils::write.csv(df, temp, row.names = FALSE)

  expect_message(mermaid_import_project_data(temp, p, method = "bleaching"), "successfully")
})

test_that("mermaid_import_project_data allows NA/NULL for bleaching percent cover observations", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(
    list(
      `Site *` = "1201",
      `Management *` = "Fisheries",
      `Sample date: Year *` = 2009,
      `Sample date: Month *` = 12,
      `Sample date: Day *` = 5,
      `Sample time` = "1:00:00 PM",
      `Depth *` = 29,
      `Quadrat size *` = 2,
      `Observer emails *` = "sharla.gelfand@gmail.com",
      `Quadrat number` = 1,
      `Hard coral % cover` = 5,
      `Macroalgae coral % cover` = NA,
      `Soft coral % cover` = 5
    ),
    row.names = 1L,
    class = c(
      "tbl_df", "tbl",
      "data.frame"
    )
  )

  p <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"

  expect_message(mermaid_import_project_data(df, p, method = "bleaching"), "successfully")
})

test_that("mermaid_import_project_data with NA in CSVs converts NAs to '' and successfully uploads", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  df <- structure(list(
    `Site *` = "1201", `Management *` = "Fake Management Organization",
    `Sample date: Year *` = 2022, `Sample date: Month *` = 6,
    `Sample date: Day *` = 15, `Sample time` = "10:01", `Depth *` = 8, `Transect number *` = 1,
    `Transect label` = NA, `Transect length surveyed *` = 50,
    `Width *` = "5m", `Fish size bin *` = 5, `Reef slope` = NA,
    Visibility = NA, Current = NA, `Relative depth` = NA,
    Tide = "falling", Notes = NA, `Observer emails *` = "sharla.gelfand@gmail.com",
    `Fish name *` = "chaetodon auriga", `Size *` = 7.5, `Count *` = 4
  ), row.names = c(
    NA,
    -1L
  ), class = c("tbl_df", "tbl", "data.frame"))

  temp <- tempfile(fileext = ".csv")

  utils::write.csv(df, temp, row.names = FALSE)

  project_id <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"
  mermaid_import_project_data(temp, project_id, "fishbelt", dryrun = FALSE)
  collect_records <- mermaid_get_project_endpoint(project_id, "collectrecords") %>%
    tidyr::unpack(data) %>%
    dplyr::select(-id, -created_on, -updated_on) %>%
    tidyr::unpack(sample_event) %>%
    dplyr::filter(sample_date == "2022-06-15") %>%
    dplyr::select(fishbelt_transect) %>%
    tidyr::unpack(fishbelt_transect)

  expect_true(all(collect_records[["label"]] == ""))
  expect_true(all(is.na(collect_records[["reef_slope"]])))
  expect_true(all(is.na(collect_records[["visibility"]])))
  expect_true(all(is.na(collect_records[["current"]])))
  expect_true(all(is.na(collect_records[["relative_depth"]])))
  expect_true(all(collect_records[["notes"]] == ""))
})

test_that("mermaid_import_project_data fails gracefully on 504", {

  # Test no longer done because timeout increased to 300s -- test would take too long, we know the messaging works

  # skip_if_offline()
  # skip_on_ci()
  # skip_on_cran()
  #
  # df <- structure(list(`Site *` = c("Ada01", "Ada01"), `Management *` = c(
  #   "Adavaci_open",
  #   "Adavaci_open"
  # ), `Sample date: Year *` = c(2017, 2017), `Sample date: Month *` = c(
  #   5,
  #   5
  # ), `Sample date: Day *` = c(15, 15), `Sample time` = structure(c(
  #   43200,
  #   43200
  # ), class = c("hms", "difftime"), units = "secs"), `Depth *` = c(
  #   8,
  #   8
  # ), `Transect number *` = c(1, 1), `Transect label` = c(NA, NA), `Transect length surveyed *` = c(50, 50), `Width *` = c(
  #   5,
  #   5
  # ), `Fish size bin *` = c(5, 5), `Reef slope` = c(NA, NA), Visibility = c(
  #   NA,
  #   NA
  # ), Current = c(NA, NA), `Relative depth` = c("Deep", "Deep"), Tide = c("falling", "falling"), Notes = c(NA, NA), `Observer emails *` = c(
  #   "wnaisilisili@wcs.org",
  #   "wnaisilisili@wcs.org"
  # ), `Fish name *` = c(
  #   "chaetodon auriga",
  #   "heniochus varius"
  # ), `Size *` = c(7.5, 7.5), `Count *` = c(
  #   4,
  #   2
  # )), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))
  #
  # for (i in 1:12) {
  #   df <- dplyr::bind_rows(df, df)
  # }
  #
  # expect_error(
  #   mermaid_import_project_data(df, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "fishbelt", dryrun = TRUE),
  #   "timed out due to the size of the data"
  # )
})
