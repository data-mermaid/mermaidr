test_that("mermaid_get_project_endpoint throws an error when no project is passed, when trying to get data from a project you don't have access to, and when an unexpected endpoint is passed", {
  skip_if_offline()
  mermaid_set_default_project("")
  expect_error(mermaid_get_project_endpoint(), "Please supply a project to get data from")
  test_project <- mermaid_search_projects("Sharla test")
  expect_error(mermaid_get_project_endpoint("bd115221-fde4-4e4c-bc73-1ce01b9d9fdc", "sites"), regexp = "Forbidden")
})

test_that("mermaid_get_project_endpoint returns a tibble when passed a known endpoint.", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  expect_is(mermaid_get_project_endpoint(test_project, "sites", limit = 1), "tbl_df")
  output <- mermaid_get_project_endpoint(test_project, "beltfishtransectmethods", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["beltfishtransectmethods"]])
  output <- mermaid_get_project_endpoint(test_project, "beltfishes", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["beltfishes"]])
  output <- mermaid_get_project_endpoint(test_project, "benthiclittransectmethods", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["benthiclittransectmethods"]])
  output <- mermaid_get_project_endpoint(test_project, "benthicpittransectmethods", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["benthicpittransectmethods"]])
  output <- mermaid_get_project_endpoint(test_project, "benthicpits", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["benthicpits"]])
  output <- mermaid_get_project_endpoint(test_project, "collectrecords", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["collectrecords"]])
  output <- mermaid_get_project_endpoint(test_project, "habitatcomplexities", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["habitatcomplexities"]])
  output <- mermaid_get_project_endpoint(test_project, "obsbenthiclits", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["obsbenthiclits"]])
  output <- mermaid_get_project_endpoint(test_project, "obsbenthicpits", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["obsbenthicpits"]])
  output <- mermaid_get_project_endpoint(test_project, "obshabitatcomplexities", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["obshabitatcomplexities"]])
  output <- mermaid_get_project_endpoint(test_project, "obstransectbeltfishs", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["obstransectbeltfishs"]])
  output <- mermaid_get_project_endpoint(test_project, "managements", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["managements_project"]])
  output <- mermaid_get_project_endpoint(test_project, "observers", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["observers"]])
  output <- mermaid_get_project_endpoint(test_project, "project_profiles", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["project_profiles"]])
  output <- mermaid_get_project_endpoint(test_project, "sampleevents", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["sampleevents"]])
  output <- mermaid_get_project_endpoint(test_project, "sites", limit = 1)
  expect_equal(names(output), mermaid_endpoint_columns[["sites"]])
})

test_that("mermaid_get_project_endpoint returns a tibble when passed a known endpoint, even if there is no data.", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  expect_is(mermaid_get_project_endpoint(test_project, "obsbenthicpits", limit = 1), "tbl_df")
})

test_that("mermaid_get_project_endpoint works for the new endpoints", {
  skip_if_offline()
  test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
  expect_s3_class(mermaid_get_project_endpoint(test_project, "beltfishes/obstransectbeltfishes/", limit = 1), "tbl_df")
  expect_s3_class(mermaid_get_project_endpoint(test_project, "beltfishes/sampleunits/", limit = 1), "tbl_df")
  expect_s3_class(mermaid_get_project_endpoint(test_project, "beltfishes/sampleevents/", limit = 1), "tbl_df")
})

test_that("mermaid_get_project_endpoint allows multiple projects, and combines the results", {
  p <- mermaid_list_my_projects(include_test_projects = TRUE)
  output <- mermaid_get_project_endpoint(p, "sites", limit = 1)
  expect_is(output, "tbl_df")

  output <- mermaid_get_project_endpoint(c("d5491b25-4a5f-401b-a50f-bb80fd1df78f", "5679ef3d-bafc-453d-9e1a-a4b282a8a997"), "sites")
  expect_is(output, "tbl_df")

  output <- mermaid_get_project_endpoint(c("d5491b25-4a5f-401b-a50f-bb80fd1df78f", "5679ef3d-bafc-453d-9e1a-a4b282a8a997"), "beltfishes/sampleunits/")
  expect_is(output, "tbl_df")

})

test_that("unpack_df_cols and repack_df_cols work as expected", {
  df <- tibble::tibble(x = tibble::tibble(a = 1, b = 2),
                       y = tibble::tibble(c = 1, d = 2),
                       z = 1)

  df_unpack <- unpack_df_cols(df)
  expect_named(df_unpack, c("x_a", "x_b", "y_c", "y_d", "z"))
  expect_true(nrow(df_unpack) == 1)

  df_repack <- repack_df_cols(df_unpack)
  expect_named(df_repack, names(df))
  expect_true(nrow(df_repack) == 1)
})

test_that("a data frame can be unpacked, rbinded, and repacked", {
  df <- tibble::tibble(x = tibble::tibble(a = 1, b = 2),
                       y = tibble::tibble(c = 1, d = 2),
                       z = 1)

  df_unpack <- unpack_df_cols(df)
  df_unpack_rbind <- df_unpack %>%
    dplyr::bind_rows(df_unpack)

  attr(df_unpack_rbind, "df_cols") <- attr(df_unpack, "df_cols")
  attr(df_unpack_rbind, "col_order") <- attr(df_unpack, "col_order")

  df_repack <- df_unpack_rbind %>%
    repack_df_cols()

  expect_named(df_repack, names(df))
  expect_true(nrow(df_repack) == nrow(df)*2)
})
