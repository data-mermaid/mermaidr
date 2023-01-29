test_that("mermaid_import_check_options fails if field is not in options", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_error(
    mermaid_import_check_options(dplyr::tibble(), options, "Site"),
    "does not exist in `options`"
  )
})

test_that("mermaid_import_check_options fails if field is Template", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_error(
    mermaid_import_check_options(dplyr::tibble(), options, "Template"),
    "is not a valid field to check"
  )
})


test_that("mermaid_import_check_options fails if field is not in data", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_error(
    mermaid_import_check_options(dplyr::tibble(), options, "Site *"),
    "does not exist in `data`"
  )
})

test_that("mermaid_import_check_options fails if required is not in options$field", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  temp <- options["Site *"]
  temp[["Site *"]][["required"]] <- NULL

  expect_error(
    mermaid_import_check_options(dplyr::tibble(`Site *` = "test"), temp, "Site *"),
    "field is missing"
  )
})

test_that("mermaid_import_check_options errors when required and NA values", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_message(
    mermaid_import_check_options(dplyr::tibble(`Count *` = NA), options, "Count *"),
    "data contains NA values"
  )
})

test_that("mermaid_import_check_options continues when NOT required and NA values", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_message(
    mermaid_import_check_options(dplyr::tibble(`Count *` = NA), options, "Count *"),
    "data contains NA values"
  )
})

test_that("mermaid_import_check_options returns message, ends when no choices present (any value allowed)", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_message(
    mermaid_import_check_options(dplyr::tibble(`Count *` = "test"), options, "Count *"),
    "Any value is allowed"
  )
})

test_that("mermaid_import_check_options returns message and table when all values match (case insensitive)", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_message(
    {
      res <- mermaid_import_check_options(dplyr::tibble(`Reef slope` = "crest"), options, "Reef slope")
    },
    "All values"
  )
  expect_identical(
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "crest", "crest", TRUE
    ),
    res
  )

  expect_message(
    {
      res <- mermaid_import_check_options(dplyr::tibble(`Reef slope` = "Crest"), options, "Reef slope")
    },
    "All values"
  )
  expect_identical(
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "Crest", "crest", TRUE
    ),
    res
  )
})

test_that("mermaid_import_check_options returns message and table when some or all values do NOT match", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  p <- "02e6915c-1c64-4d2c-bac0-326b560415a2"
  options <- mermaid_import_get_options(p, "fishbelt")

  expect_message(
    {
      res <- mermaid_import_check_options(dplyr::tibble(`Reef slope` = c("crest", "wal")), options, "Reef slope")
    },
    "Some errors"
  )
  expect_identical(
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "wal", "wall", FALSE,
      "crest", "crest", TRUE
    ),
    res
  )
})

test_that("closest_string_match is not case sensitive", {
  res <- tibble::tibble(data_value = "TEST") %>% closest_string_match(tibble::tibble(choices = "test"))
  expect_identical(
    res,
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "TEST", "test", TRUE
    )
  )
})

test_that("closest_string_match works", {
  res <- tibble::tibble(data_value = c("test", "toss")) %>% closest_string_match(tibble::tibble(choices = c("test", "tess", "tests", "ross")))
  expect_identical(
    res,
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "test", "test", TRUE,
      "toss", "tess", FALSE
    )
  )
})

test_that("closest_string_match returns in same order passed", {
  res <- tibble::tibble(data_value = c("toss", "test")) %>% closest_string_match(tibble::tibble(choices = c("test", "tess", "tests", "ross")))
  expect_identical(
    res,
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "toss", "tess", FALSE,
      "test", "test", TRUE
    )
  )
})

test_that("closest_string_match de-duplicates", {
  res <- tibble::tibble(data_value = c("toss", "toss", "test")) %>% closest_string_match(tibble::tibble(choices = c("test", "tess", "tests", "ross")))
  expect_identical(
    res,
    tibble::tribble(
      ~data_value, ~closest_choice, ~match,
      "toss", "tess", FALSE,
      "test", "test", TRUE
    )
  )
})
