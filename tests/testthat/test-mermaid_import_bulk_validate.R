test_that("mermaid_import_bulk_validate gives a message when there are no records to validate", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_output(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_validate(),
    "No records in Collecting to validate."
  )
})

test_that("mermaid_import_bulk_validate errors when you are not a member of the project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_projects()[, "id"]
  my_p <- mermaid_get_my_projects()[, "id"]

  not_my_p <- p %>%
    dplyr::anti_join(my_p, by = "id") %>%
    head(1)

  expect_error(mermaid_import_bulk_validate(not_my_p), "You are not a member of this project.")
})

test_that("validate_collect_records handles errors in sending the request", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    validate_collect_records(dplyr::tibble(x = "test"), mermaid_get_my_projects(limit = 1)[["id"]]),
    "Internal Server Error"
  )
})

test_that("mermaid_import_bulk_validate returns the expected messaging", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # Ensure there are records to validate
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

  mermaid_import_project_data(df, project_id, "fishbelt", dryrun = FALSE)

  expect_output(mermaid_import_bulk_validate(project_id), "(?s)^(?=.*being validated)", perl = TRUE)
})

test_that("mermaid_import_bulk_validate validates all collecting records", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # Ensure there are records to validate
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

  mermaid_import_project_data(df, project_id, "fishbelt", dryrun = FALSE)

  # some validations_status are NA before
  expect_true(get_collecting_records(project_id) %>%
    dplyr::filter(!is.na(validations_status)) %>%
    nrow() > 1)

  mermaid_import_bulk_validate(project_id)

  # No validations_status are NA after
  expect_true(get_collecting_records(project_id) %>%
    dplyr::filter(is.na(validations_status)) %>%
    nrow() == 0)

  # None are stale after
  expect_true(get_collecting_records(project_id) %>%
    dplyr::filter(validations_status == "stale") %>%
    nrow() == 0)
})
