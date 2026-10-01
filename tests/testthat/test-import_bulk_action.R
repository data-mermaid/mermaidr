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
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

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
