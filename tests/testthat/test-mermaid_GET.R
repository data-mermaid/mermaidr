test_that("mermaid_GET throws error when endpoint does not exist", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_GET("slgkjgs"), regexp = "Mermaid API request failed: (404) Not Found", fixed = TRUE)
})

test_that("mermaid_GET returns projects using showall query parameter", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_equal(nrow(mermaid_GET("projects", limit = 1)[["projects"]]), 1)
})

test_that("mermaid_GET returns a tibble with column data of nested tibbles if the endpoint is choices", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_GET("choices")[["choices"]]
  expect_s3_class(output, "tbl_df")
  expect_s3_class(output[["data"]][[1]], "tbl_df")
})

test_that("mermaid_GET allows the return of two endpoints as a named list", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_GET(c("choices", "sites"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("choices", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))

  output <- mermaid_GET(c("sites", "sites"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))
})

test_that("mermaid_GET returns a single endpoint in a named list", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_GET("sites", limit = 1)
  expect_is(output, "list")
  expect_named(output, "sites")
})

test_that("suppress_http_warning suppresses warnings from `warning_function` with `warning_regex` only", {
  expect_silent(suppress_http_warning(httr:::parse_single_header("HTTP_API_VERSION: v0.19.2")))
  expect_warning(suppress_http_warning(httr:::parse_single_header("HTTP_API_VERSION: v0.19.2"), "other_function"))
  expect_warning(suppress_http_warning(httr:::parse_single_header(c("HTTP/1.1 200 OK", "A: B", "Invalid"))), "Failed to parse headers")
})

test_that("suppress_http_warning suppresses HTTP warnings", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  # expect_warning(httr::GET("https://dev-api.datamermaid.org/v1/projects/"))
  expect_silent(suppress_http_warning(httr::GET("https://dev-api.datamermaid.org/v1/projects/")))
})

test_that("expand_covariates pulls max value and can handle both covariates present, one missing, both missing, and just NULL values", {
  both_present <- dplyr::tibble(
    id = c(1, 1),
    name = c("name_1", "name_2"),
    value = list(
      dplyr::tibble(name = c("subname_1", "subname_2"), area = c(1, 2)),
      dplyr::tibble(name = c("subname_3", "subname_4"), area = c(3, 4))
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 1)

  one_missing <- dplyr::tibble(
    id = c(1, 1),
    name = c("name_1", "name_2"),
    value = list(
      dplyr::tibble(name = c("subname_1", "subname_2"), area = c(1, 2)),
      dplyr::tibble()
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 2)

  both_missing <- dplyr::tibble(
    id = c(1, 1),
    name = c("name_1", "name_2"),
    value = list(
      dplyr::tibble(),
      dplyr::tibble()
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 3)

  covariates_null <- dplyr::tibble(covariates = NULL, id = 4)

  covariates_df <- both_present %>%
    dplyr::bind_rows(one_missing) %>%
    dplyr::bind_rows(both_missing) %>%
    dplyr::bind_rows(covariates_null)

  covariates_expanded <- covariates_df %>%
    dplyr::mutate(covariates = purrr::map(covariates, expand_covariates)) %>%
    tidyr::unnest(covariates) %>%
    dplyr::select(-dplyr::any_of("covariates"))

  expected <- tibble::tribble(
    ~name_1, ~name_2, ~id,
    "subname_2", "subname_4", 1,
    "subname_2", NA_character_, 2,
    NA_character_, NA_character_, 3,
    NA_character_, NA_character_, 4
  )

  expect_identical(covariates_expanded, expected)

  expect_error(covariates_df %>%
    dplyr::slice(1:3) %>%
    dplyr::mutate(covariates = purrr::map(covariates, expand_covariates)) %>%
    tidyr::unnest(covariates) %>%
    dplyr::select(-.data$covariates))
})
