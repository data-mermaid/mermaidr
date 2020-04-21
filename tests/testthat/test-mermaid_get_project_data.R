test_that("mermaid_get_project_data returns a data frame with the correct names", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = "benthicpits", data = "sampleunits", limit = 1)
  expect_named(output, project_data_columns[["benthicpits/sampleunits"]])
  expect_true(nrow(output) == 1)
  expect_is(output, "tbl_df")
})

test_that("mermaid_get_project_data allows multiple methods", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = c("beltfishes", "benthicpits"), data = "sampleunits", limit = 1)
  expect_named(output, c("beltfishes", "benthicpits"))
})

test_that("mermaid_get_project_data allows multiple forms of data", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = "beltfishes", data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("observations", "sampleunits", "sampleevents"))
})

test_that("mermaid_get_project_data allows multiple methods and multiple forms of data", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 1)
  output <- mermaid_get_project_data(p, method = c("beltfishes", "benthicpits"), data = c("observations", "sampleunits", "sampleevents"), limit = 1)
  expect_named(output, c("beltfishes", "benthicpits"))
  expect_named(output[["beltfishes"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpits"]], c("observations", "sampleunits", "sampleevents"))
  expect_named(output[["benthicpits"]][["sampleunits"]], project_data_columns[["benthicpits/sampleunits"]])
  expect_named(output[["beltfishes"]][["observations"]], project_data_columns[["beltfishes/obstransectbeltfishes"]])
})

test_that("mermaid_get_project_data errors if passed a wrong method or data", {
  skip_if_offline()
  p <- mermaid_get_my_projects(limit = 1)
  expect_error(mermaid_get_project_data(p, method = "beltfishs", data = "sampleunits"), "one of")
  expect_error(mermaid_get_project_data(p, method = "beltfishes", data = "samplevents"), "one of")
})
