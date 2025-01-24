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

test_that("mermaid_import_bulk_submit gives a message when there are no records to submit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_output(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_submit(),
    "No valid records in Collecting to submit. Have you run `mermaid_import_bulk_validate()`?",
    fixed = TRUE
  )
})

test_that("mermaid_import_bulk_edit errors when you do not give a valid method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_edit(),
    "`method` must be one of"
  )

  expect_error(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_edit("all"),
    "`method` must be one of"
  )
})

test_that("mermaid_import_bulk_edit errors when you give multiple methods", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_edit(c("fishbelt", "benthicpqt")),
    "`method` must be one of"
  )
})

test_that("mermaid_import_bulk_edit gives a message when there are no records to edit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_output(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_edit("fishbelt"),
    "No submitted records to edit"
  )
})

test_that("mermaid_import_bulk_ functions error when you are not a member of the project", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- mermaid_get_projects()[, "id"]
  my_p <- mermaid_get_my_projects()[, "id"]

  not_my_p <- p %>%
    dplyr::anti_join(my_p, by = "id") %>%
    head(1)

  expect_error(mermaid_import_bulk_validate(not_my_p), "You are not a member of this project.")
  expect_error(mermaid_import_bulk_submit(not_my_p), "You are not a member of this project.")
  expect_error(mermaid_import_bulk_edit(not_my_p, "fishbelt"), "Forbidden")
})

test_that("validate_or_submit_collect_records handles errors in sending the request, returns error for validate", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    validate_or_submit_collect_records(dplyr::tibble(x = "test"), mermaid_get_my_projects(limit = 1)[["id"]], "validate"),
    "Bad Request"
  )
})

test_that("validate_or_submit_collect_records handles errors in sending the request, returns 'not_ok' result for submit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_equal(
    validate_or_submit_collect_records(dplyr::tibble(x = "test"), mermaid_get_my_projects(limit = 1)[["id"]], "submit"),
    dplyr::tibble(status = "not_ok")
  )
})

test_that("edit_records handles errors in sending the request, returns 'not_ok' result", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_equal(
    edit_records(dplyr::tibble(x = "test"), mermaid_get_my_projects(limit = 1)[["id"]], "beltfishtransectmethods"),
    dplyr::tibble(status = "not_ok")
  )
})

test_that("Entire validate/submit/edit flow works", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"

  # Query collect records to get the ID of the one we are about to submit (via diff)
  collecting_records_before <- get_collecting_records(p)

  # Ensure there are VALID records to submit - create and validate them
  df <- tibble::tribble(
    ~`Site *`, ~`Management *`, ~`Sample date: Year *`, ~`Sample date: Month *`, ~`Sample date: Day *`, ~`Sample time`, ~`Depth *`, ~`Transect number *`, ~`Transect label`, ~`Transect length surveyed *`, ~`Width *`, ~`Fish size bin *`, ~`Reef slope`, ~`Visibility`, ~`Current`, ~`Relative depth`, ~`Tide`, ~`Notes`, ~`Observer emails *`, ~`Fish name *`, ~`Size *`, ~`Count *`,
    "bulk submit test site", "bulk submit test mr", 2017, 5, 15, "10:01", 8, 1, NA, 50, "5m", 5, NA, NA, NA, "Deep", "falling", NA, "sharla.gelfand@gmail.com", "chaetodon auriga", 7.5, 24,
    "bulk submit test site", "bulk submit test mr", 2017, 5, 15, "10:01", 8, 1, NA, 50, "5m", 5, NA, NA, NA, "Deep", "falling", NA, "sharla.gelfand@gmail.com", "Fibramia", 7.5, 46,
    "bulk submit test site", "bulk submit test mr", 2017, 5, 15, "10:01", 8, 1, NA, 50, "5m", 5, NA, NA, NA, "Deep", "falling", NA, "sharla.gelfand@gmail.com", "Jaydia", 7.5, 54,
    "bulk submit test site", "bulk submit test mr", 2017, 5, 15, "10:01", 8, 1, NA, 50, "5m", 5, NA, NA, NA, "Deep", "falling", NA, "sharla.gelfand@gmail.com", "Acanthurus fowleri", 7.5, 100,
    "bulk submit test site", "bulk submit test mr", 2017, 5, 15, "10:01", 8, 1, NA, 50, "5m", 5, NA, NA, NA, "Deep", "falling", NA, "sharla.gelfand@gmail.com", "Chrysiptera brownriggii", 7.5, 40,
  )

  import_project_data_internal(df, p, "fishbelt", dryrun = FALSE, clearexisting = TRUE, clearexistingforce = TRUE)

  # Query collect records, get ID, check there it at least 1 valid record
  collecting_records_after_import <- get_collecting_records(p)

  new_collect_record_id <- collecting_records_after_import %>%
    dplyr::anti_join(
      collecting_records_before %>%
        dplyr::select(id),
      by = "id"
    ) %>%
    dplyr::pull(id)

  # some validations_status are NA before submitting
  expect_true(get_collecting_records(p) %>%
    dplyr::filter(!is.na(validations_status)) %>%
    nrow() > 0)

  mermaid_import_bulk_validate(p)

  # No validations_status are NA after
  expect_true(get_collecting_records(p) %>%
    dplyr::filter(is.na(validations_status)) %>%
    nrow() == 0)

  # None are stale after
  expect_true(get_collecting_records(p) %>%
    dplyr::filter(validations_status == "stale") %>%
    nrow() == 0)

  collecting_records_after_validate <- get_collecting_records(p)
  new_collect_record_after_validating <- collecting_records_after_validate %>%
    dplyr::filter(id == new_collect_record_id)
  expect_equal(new_collect_record_after_validating[["validations_status"]], "ok")

  # Query how many SUs of each method
  method_sus <- new_collect_record_after_validating %>%
    dplyr::distinct(data_protocol) %>%
    split(.$data_protocol) %>%
    purrr::imap(\(x, method) {
      methods_endpoint <- method_to_methods_endpoint(method)
      mermaid_get_project_endpoint(p, methods_endpoint)
    })

  # Submit
  mermaid_import_bulk_submit(p)

  # Check there are no valid records in Collecting
  collecting_records_after_submit <- get_collecting_records(p)

  expect_true(collecting_records_after_submit %>%
    dplyr::filter(validations_status == "ok") %>%
    nrow() == 0)

  # Check there are the same number of non-valid records
  expect_identical(
    collecting_records_after_validate %>% dplyr::filter(validations_status != "ok"),
    collecting_records_after_submit %>% dplyr::filter(validations_status != "ok")
  )

  # Check there are new SUs of those methods now
  method_sus_after <- new_collect_record_after_validating %>%
    dplyr::distinct(data_protocol) %>%
    split(.$data_protocol) %>%
    purrr::imap(\(x, method) {
      methods_endpoint <- method_to_methods_endpoint(method)
      mermaid_get_project_endpoint(p, methods_endpoint)
    })

  purrr::walk(
    names(method_sus),
    \(method) {
      expect_true(
        nrow(method_sus_after[[method]]) > nrow(method_sus[[method]])
      )
    }
  )

  # Check that mermaid_import_bulk_edit gives a message requiring confirmation
  expect_error(
    expect_message(mermaid_import_bulk_edit(p, "fishbelt"), "This will move ALL existing submitted fishbelt records"),
    "not interactive"
  )

  # Move record back into editing so the test can run again without errors that the SUs are identical
  import_bulk_action(p, action = "edit", method = "fishbelt", bulkeditforce = TRUE)

  # Check that there are no more fishbelt records submitted
  method_sus_after_editing <- new_collect_record_after_validating %>%
    dplyr::distinct(data_protocol) %>%
    split(.$data_protocol) %>%
    purrr::imap(\(x, method) {
      methods_endpoint <- method_to_methods_endpoint(method)
      mermaid_get_project_endpoint(p, methods_endpoint)
    })

  purrr::walk(
    names(method_sus),
    \(method) {
      expect_true(
        nrow(method_sus_after_editing[[method]]) == 0
      )
    }
  )

  # Check that they are back in collecting, and they are all stale
  collecting_records_after_edit <- get_collecting_records(p)

  expect_identical(
    collecting_records_after_edit %>%
      dplyr::filter(data_protocol == "fishbelt") %>%
      dplyr::pull(validations_status) %>%
      unique(),
    "stale"
  )
})

test_that("summarise_single_status returns the correct messaging", {
  expect_message(
    summarise_single_status(dplyr::tibble(status = "warning", n = 1), action = "validate", ""),
    "1 record produced warnings in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "warning", n = 0), action = "validate", ""),
    "0 records produced warnings in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "warning", n = 2), action = "validate", ""),
    "2 records produced warnings in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "error", n = 1), action = "validate", ""),
    "1 record produced errors in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "error", n = 0), action = "validate", ""),
    "0 records produced errors in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "error", n = 2), action = "validate", ""),
    "2 records produced errors in validation"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "ok", n = 1), action = "validate", ""),
    "1 record successfully validated without warnings or errors"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "ok", n = 0), action = "validate", ""),
    "0 records successfully validated without warnings or errors"
  )

  expect_message(
    summarise_single_status(dplyr::tibble(status = "ok", n = 2), action = "validate", ""),
    "2 records successfully validated without warnings or errors"
  )
})

test_that("summarise_all_validations_statuses returns the correct messaging", {
  res <- dplyr::tribble(
    ~status, ~n,
    "warning", 2,
    "error", 4,
    "ok", 1
  )

  local_edition(3)

  expect_snapshot(res %>%
    tidyr::uncount(weights = n) %>%
    summarise_all_statuses(c("error", "warning", "ok"), "validate", "NONE"))
})
