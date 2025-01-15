test_that("Message when none to submit gives a message when there are no records to submit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_output(
    mermaid_search_my_projects("empty project test",
      include_test_projects = TRUE
    ) %>%
      mermaid_import_bulk_submit(),
    "No valid records in Collecting to submit."
  )
})

test_that("validate_or_submit_collect_records handles errors in sending the request", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    validate_or_submit_collect_records(
      dplyr::tibble(id = "test"),
      mermaid_get_my_projects(limit = 1)[["id"]],
      action = "submit"
    ),
    "Internal Server Error"
  )
})

test_that("mermaid_import_bulk_submit submits all valid collecting records, then mermaid_import_bulk_edit moves them back to collecting", {
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

  mermaid_import_bulk_validate(p)

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

  # Move record back into editing so the test can run again without errors that the SUs are identical
})
