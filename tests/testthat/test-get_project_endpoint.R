test_that("get_project_endpoint throws an error when no project is passed, when trying to get data from a project you don't have access to, and when an unexpected endpoint is passed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  mermaid_set_default_project("")
  expect_error(get_project_endpoint(), "Please supply a project to get data from")
  expect_error(get_project_endpoint("bd115221-fde4-4e4c-bc73-1ce01b9d9fdc", "sites"), regexp = "Forbidden")
})

test_that("get_project_endpoint returns a tibble with specified names when passed a known endpoint.", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  test_project <- "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"
  expect_named(get_project_endpoint(test_project, "beltfishtransectmethods", limit = 1), mermaid_project_endpoint_columns[["beltfishtransectmethods"]])
})

test_that("get_project_endpoint returns a tibble when passed a known endpoint, even if there is no data.", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # TODO
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  expect_named(get_project_endpoint(test_project, "benthictransects", limit = 1), mermaid_project_endpoint_columns[["benthictransects"]])
})

test_that("get_project_endpoint allows multiple projects, and combines the results, adding a project identifier", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  p <- mermaid_get_my_projects()
  expect_named(get_project_endpoint(p, "sites", limit = 1), c("project", cols_without_covars(mermaid_project_endpoint_columns[["sites"]], covars_cols)))

  p <- c("9de82789-c38e-462e-a1a8-e02c020c7a35", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a", "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b")
  expect_named(get_project_endpoint(p, "beltfishtransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["beltfishtransectmethods"]]))
  expect_named(get_project_endpoint(p, "benthiclittransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["benthiclittransectmethods"]]))
  expect_named(get_project_endpoint(p, "benthicpittransectmethods", limit = 1), c("project_id", mermaid_project_endpoint_columns[["benthicpittransectmethods"]]))
  expect_named(get_project_endpoint(p, "fishbelttransects", limit = 1), c("project_id", mermaid_project_endpoint_columns[["fishbelttransects"]]))
  expect_named(get_project_endpoint(p, "managements", limit = 1), c("project_id", mermaid_project_endpoint_columns[["managements"]]))
  expect_named(get_project_endpoint(p, "observers", limit = 1), c("project_id", mermaid_project_endpoint_columns[["observers"]]))
  expect_named(get_project_endpoint(p, "project_profiles", limit = 1), c("project_id", mermaid_project_endpoint_columns[["project_profiles"]]))
  expect_named(get_project_endpoint(p, "sampleevents", limit = 1), c("project_id", mermaid_project_endpoint_columns[["sampleevents"]]))
  expect_named(get_project_endpoint(p, "sites", limit = 1), c("project_id", cols_without_covars(mermaid_project_endpoint_columns[["sites"]], covars_cols)))

  expect_named(get_project_endpoint(p, "beltfishes/obstransectbeltfishes", limit = 1), c(mermaid_project_endpoint_columns[["beltfishes/obstransectbeltfishes"]]))
  sus <- get_project_endpoint(p, "beltfishes/sampleunits", limit = 1)
  expect_true(all(project_data_test_columns[["beltfishes/sampleunits"]] %in% names(sus)))
  expect_true(any(stringr::str_starts(names(sus), project_data_df_columns_list_names[["beltfishes/sampleunits"]])))
  ses <- get_project_endpoint(p, "beltfishes/sampleevents", limit = 1)
  expect_true(all(project_data_test_columns[["beltfishes/sampleevents"]] %in% names(ses)))
  expect_true(any(stringr::str_starts(names(ses), project_data_df_columns_list_names[["beltfishes/sampleevents"]])))

  expect_named(get_project_endpoint(p, "benthicpits/obstransectbenthicpits", limit = 1), c(mermaid_project_endpoint_columns[["benthicpits/obstransectbenthicpits"]]))
  sus <- get_project_endpoint(p, "benthicpits/sampleunits", limit = 1)
  expect_true(all(project_data_test_columns[["benthicpits/sampleunits"]] %in% names(sus)))
  expect_true(any(stringr::str_starts(names(sus), project_data_df_columns_list_names[["benthicpits/sampleunits"]])))
  ses <- get_project_endpoint(p, "benthicpits/sampleevents", limit = 1)
  expect_true(all(project_data_test_columns[["benthicpits/sampleevents"]] %in% names(ses)))
  expect_true(any(stringr::str_starts(names(ses), project_data_df_columns_list_names[["benthicpits/sampleevents"]])))

  expect_named(get_project_endpoint(p, "benthiclits/obstransectbenthiclits", limit = 1), c(mermaid_project_endpoint_columns[["benthiclits/obstransectbenthiclits"]]))
  sus <- get_project_endpoint(p, "benthiclits/sampleunits", limit = 1)
  expect_true(all(project_data_test_columns[["benthiclits/sampleunits"]] %in% names(sus)))
  expect_true(any(stringr::str_starts(names(sus), project_data_df_columns_list_names[["benthiclits/sampleunits"]])))
  ses <- get_project_endpoint(p, "benthiclits/sampleevents", limit = 1)
  expect_true(all(project_data_test_columns[["benthiclits/sampleevents"]] %in% names(ses)))
  expect_true(any(stringr::str_starts(names(ses), project_data_df_columns_list_names[["benthiclits/sampleevents"]])))

  expect_named(get_project_endpoint(p, "habitatcomplexities/obshabitatcomplexities", limit = 1), c(mermaid_project_endpoint_columns[["habitatcomplexities/obshabitatcomplexities"]]))
  expect_named(get_project_endpoint(p, "habitatcomplexities/sampleunits", limit = 1), c(mermaid_project_endpoint_columns[["habitatcomplexities/sampleunits"]]))
  expect_named(get_project_endpoint(p, "habitatcomplexities/sampleevents", limit = 1), c(mermaid_project_endpoint_columns[["habitatcomplexities/sampleevents"]]))

  expect_named(get_project_endpoint(p, "bleachingqcs/obscoloniesbleacheds", limit = 1), c(mermaid_project_endpoint_columns[["bleachingqcs/obscoloniesbleacheds"]]))
  expect_named(get_project_endpoint(p, "bleachingqcs/obsquadratbenthicpercents", limit = 1), c(mermaid_project_endpoint_columns[["bleachingqcs/obsquadratbenthicpercents"]]))
  expect_named(get_project_endpoint(p, "bleachingqcs/sampleunits", limit = 1), c(mermaid_project_endpoint_columns[["bleachingqcs/sampleunits"]]))
  expect_named(get_project_endpoint(p, "bleachingqcs/sampleevents", limit = 1), c(mermaid_project_endpoint_columns[["bleachingqcs/sampleevents"]]))
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
  skip_on_ci()
  skip_on_cran()
  output <- get_project_endpoint("9de82789-c38e-462e-a1a8-e02c020c7a35", endpoint = "benthicpits/obstransectbenthicpits")
  expect_true(nrow(output) > 5000)
})

test_that("get_project_endpoint allows multiple projects and multiple endpoints", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- get_project_endpoint(c("5679ef3d-bafc-453d-9e1a-a4b282a8a997", "3a9ecb7c-f908-4262-8769-1b4dbb0cf61a"), c("sites", "managements"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "managements"))
})

test_that("sample_date is converted to a date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- get_project_endpoint("5679ef3d-bafc-453d-9e1a-a4b282a8a997", "beltfishes/obstransectbeltfishes", limit = 1)
  expect_true(inherits(output[["sample_date"]], "Date"))
})
