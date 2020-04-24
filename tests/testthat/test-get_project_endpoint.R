test_that("get_project_endpoint throws an error when no project is passed, when trying to get data from a project you don't have access to, and when an unexpected endpoint is passed", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  mermaid_set_default_project("")
  expect_error(get_project_endpoint(), "Please supply a project to get data from")
  expect_error(get_project_endpoint("bd115221-fde4-4e4c-bc73-1ce01b9d9fdc", "sites"), regexp = "Forbidden")
})

test_that("get_project_endpoint returns a tibble with specified names when passed a known endpoint.", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  test_project <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"
  expect_named(get_project_endpoint(test_project, "beltfishtransectmethods", limit = 1), mermaid_project_endpoint_columns[["beltfishtransectmethods"]])
})

test_that("get_project_endpoint returns a tibble when passed a known endpoint, even if there is no data.", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  expect_named(get_project_endpoint(test_project, "obsbenthicpits", limit = 1), mermaid_project_endpoint_columns[["obsbenthicpits"]])
})

test_that("get_project_endpoint allows multiple projects, and combines the results, adding a project identifier", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  p <- mermaid_get_my_projects(include_test_projects = TRUE)
  expect_named(get_project_endpoint(p, "sites", limit = 1), c("project", mermaid_project_endpoint_columns[["sites"]]))
  expect_named(get_project_endpoint(p, "beltfishes/sampleunits", limit = 1), c(mermaid_project_endpoint_columns[["beltfishes/sampleunits"]]))

  p <- c("9de82789-c38e-462e-a1a8-e02c020c7a35", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a")
  expect_named(get_project_endpoint(p, "beltfishtransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["beltfishtransectmethods"]]))
  expect_named(get_project_endpoint(p, "beltfishes", limit = 1), c("project_id", mermaid_project_endpoint_columns[["beltfishes"]]))
  expect_named(get_project_endpoint(p, "benthiclittransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["benthiclittransectmethods"]]))
  expect_named(get_project_endpoint(p, "benthicpittransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["benthicpittransectmethods"]]))
  expect_named(get_project_endpoint(p, "benthicpits", limit = 1), c("project_id", mermaid_project_endpoint_columns[["benthicpits"]]))
  #expect_named(get_project_endpoint(p, "collectrecords", limit = 1), c("project_id", mermaid_project_endpoint_columns[["collectrecords"]]))
  expect_named(get_project_endpoint(p, "fishbelttransects", limit = 1), c("project_id", mermaid_project_endpoint_columns[["fishbelttransects"]]))
  expect_named(get_project_endpoint(p, "habitatcomplexities", limit = 1), c("project_id", mermaid_project_endpoint_columns[["habitatcomplexities"]]))
  expect_named(get_project_endpoint(p, "obsbenthiclits", limit = 1), c("project_id", mermaid_project_endpoint_columns[["obsbenthiclits"]]))
  expect_named(get_project_endpoint(p, "obsbenthicpits", limit = 1), c("project_id", mermaid_project_endpoint_columns[["obsbenthicpits"]]))
  expect_named(get_project_endpoint(p, "obshabitatcomplexities", limit = 1), c("project_id", mermaid_project_endpoint_columns[["obshabitatcomplexities"]]))
  expect_named(get_project_endpoint(p, "obstransectbeltfishs", limit = 1), c("project_id", mermaid_project_endpoint_columns[["obstransectbeltfishs"]]))
  expect_named(get_project_endpoint(p, "managements", limit = 1), c("project_id", mermaid_project_endpoint_columns[["managements"]]))
  expect_named(get_project_endpoint(p, "observers", limit = 1), c("project_id", mermaid_project_endpoint_columns[["observers"]]))
  expect_named(get_project_endpoint(p, "project_profiles", limit = 1), c("project_id", mermaid_project_endpoint_columns[["project_profiles"]]))
  expect_named(get_project_endpoint(p, "sampleevents", limit = 1), c("project_id", mermaid_project_endpoint_columns[["sampleevents"]]))
  expect_named(get_project_endpoint(p, "sites", limit = 1), c("project_id", mermaid_project_endpoint_columns[["sites"]]))
  expect_named(get_project_endpoint(p, "beltfishes/obstransectbeltfishes", limit = 1), c(mermaid_project_endpoint_columns[["beltfishes/obstransectbeltfishes"]]))
  expect_named(get_project_endpoint(p, "beltfishes/sampleunits", limit = 1), c(mermaid_project_endpoint_columns[["beltfishes/sampleunits"]]))
  expect_named(get_project_endpoint(p, "beltfishes/sampleevents", limit = 1), c(mermaid_project_endpoint_columns[["beltfishes/sampleevents"]]))
  expect_named(get_project_endpoint(p, "benthicpits/obstransectbenthicpits", limit = 1), c(mermaid_project_endpoint_columns[["benthicpits/obstransectbenthicpits"]]))
  expect_named(get_project_endpoint(p, "benthicpits/sampleunits", limit = 1), c(mermaid_project_endpoint_columns[["benthicpits/sampleunits"]]))
  expect_named(get_project_endpoint(p, "benthicpits/sampleevents", limit = 1), c(mermaid_project_endpoint_columns[["benthicpits/sampleevents"]]))
})

test_that("unpack_df_cols and repack_df_cols work as expected", {
  df <- tibble::tibble(
    x = tibble::tibble(a = 1, b = 2),
    y = tibble::tibble(c = 1, d = 2),
    z = 1
  )

  df_unpack <- unpack_df_cols(df)
  expect_named(df_unpack, c("x_a", "x_b", "y_c", "y_d", "z"))
  expect_true(nrow(df_unpack) == 1)

  df_repack <- repack_df_cols(df_unpack)
  expect_named(df_repack, names(df))
  expect_true(nrow(df_repack) == 1)
})

test_that("a data frame can be unpacked, rbinded, and repacked", {
  df <- tibble::tibble(
    x = tibble::tibble(a = 1, b = 2),
    y = tibble::tibble(c = 1, d = 2),
    z = 1
  )

  df_unpack <- unpack_df_cols(df)
  df_unpack_rbind <- df_unpack %>%
    dplyr::bind_rows(df_unpack)

  attr(df_unpack_rbind, "df_cols") <- attr(df_unpack, "df_cols")
  attr(df_unpack_rbind, "col_order") <- attr(df_unpack, "col_order")

  df_repack <- df_unpack_rbind %>%
    repack_df_cols()

  expect_named(df_repack, names(df))
  expect_true(nrow(df_repack) == nrow(df) * 2)
})

test_that("pagination works to return all records", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  output <- get_project_endpoint("9de82789-c38e-462e-a1a8-e02c020c7a35", endpoint = "benthicpits/obstransectbenthicpits")
  expect_true(nrow(output) > 5000)
})

test_that("get_project_endpoint allows multiple projects and multiple endpoints", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  output <- get_project_endpoint(c("5679ef3d-bafc-453d-9e1a-a4b282a8a997", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"), c("sites", "managements"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "managements"))
})

test_that("sample_date is converted to a date", {
  skip_if_offline()
  skip_on_travis()
  skip_on_cran()
  output <- get_project_endpoint("5679ef3d-bafc-453d-9e1a-a4b282a8a997", "beltfishes/obstransectbeltfishes", limit = 1)
  expect_true(inherits(output[["sample_date"]], "Date"))
})