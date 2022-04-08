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
  output <- mermaid_GET(c("choices", "sites"), limit = 1, token = mermaid_token())
  expect_is(output, "list")
  expect_named(output, c("choices", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))

  output <- mermaid_GET(c("sites", "sites"), limit = 1, token = mermaid_token())
  expect_is(output, "list")
  expect_named(output, c("sites", "sites"))
  expect_equal(unname(unlist(lapply(output, nrow))), c(1, 1))
})

test_that("mermaid_GET returns a single endpoint in a named list", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_GET("sites", limit = 1, token = mermaid_token())
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

test_that("expand_covariates pulls max value when there are multiples, and can handle all covariates present, some missing, all missing, just NULL values, some numeric, etc", {
  all_present <- dplyr::tibble(
    id = c(1, 1, 1),
    name = c("aca_name_1", "aca_name_2", "cov_name_2"),
    value = list(
      dplyr::tibble(name = c("subname_1", "subname_2"), area = c(1, 2)),
      dplyr::tibble(name = c("subname_3", "subname_4"), area = c(3, 4)),
      5
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 1)

  one_missing <- dplyr::tibble(
    id = c(1, 1, 1),
    name = c("aca_name_1", "aca_name_2", "cov_name_2"),
    value = list(
      dplyr::tibble(name = c("subname_1", "subname_2"), area = c(1, 2)),
      dplyr::tibble(),
      5
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 2)

  all_missing <- dplyr::tibble(
    id = c(1, 1, 1),
    name = c("aca_name_1", "aca_name_2", "cov_name_2"),
    value = list(
      dplyr::tibble(),
      dplyr::tibble(),
      NULL
    )
  ) %>%
    tidyr::nest(covariates = c(id, name, value)) %>%
    dplyr::mutate(id = 3)

  covariates_null <- dplyr::tibble(covariates = NULL, id = 4)

  covariates_df <- all_present %>%
    dplyr::bind_rows(one_missing) %>%
    dplyr::bind_rows(all_missing) %>%
    dplyr::bind_rows(covariates_null)

  covariates_expanded <- covariates_df %>%
    extract_covariates()

  expected <- tibble::tribble(
    ~id, ~aca_name_1, ~aca_name_2, ~cov_name_2,
    1, "subname_2", "subname_4", 5,
    2, "subname_2", NA_character_, 5,
    3, NA_character_, NA_character_, NA_real_,
    4, NA_character_, NA_character_, NA_real_
  )

  expect_identical(covariates_expanded, expected)

  actual_covariates_null <- structure(list(covariates = list(structure(list(name = c(
    "aca_benthic", "aca_geomorphic",
    "beyer_score", "beyer_scorecn", "beyer_scorecy"
  ), value = c(
    NA, NA, 0.2, 0.3, 0.1
  )), class = "data.frame", row.names = c(
    NA,
    -5L
  )), structure(list(name = c(
    "aca_benthic", "aca_geomorphic",
    "beyer_score", "beyer_scorecn", "beyer_scorecy"
  ), value = list(
    structure(list(area = 1950.7324, name = "Seagrass"), class = "data.frame", row.names = 1L),
    structure(list(area = 1950.7324, name = "Plateau"), class = "data.frame", row.names = 1L),
    0.2, NULL, NULL
  )), class = "data.frame", row.names = c(
    NA,
    -5L
  ))), id = 1:2), row.names = c(NA, -2L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))

  covariates_expanded <- actual_covariates_null %>%
    extract_covariates()

  expected <- tibble::tribble(
    ~id, ~aca_benthic, ~aca_geomorphic, ~beyer_score, ~beyer_scorecn, ~beyer_scorecy,
    1L,           NA,              NA,          0.2,            0.3,            0.1,
    2L,   "Seagrass",       "Plateau",          0.2,             NA,             NA
  )

  expect_identical(covariates_expanded, expected)
})
