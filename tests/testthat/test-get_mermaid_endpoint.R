context("test-get_mermaid_endpoint")

test_that("get_mermaid_endpoint throws an error when an unexpected endpoint is passed", {
  expect_error(get_mermaid_endpoint("ldkgjdgk"), "endpoint must be one of 'benthicattributes', 'fishattributes', 'fishfamilies', 'fishgenera', 'fishspecies', 'managements', 'projects', 'sites'.", fixed = TRUE)
})

test_that("get_mermaid_endpoint returns a tibble when passed a known endpoint.", {
  skip_if_offline()
  expect_is(get_mermaid_endpoint("sites"), "tbl_df")
})

test_that("get_mermaid_endpoint returns the correct columns given the endpoint.", {
  skip_if_offline()
  output <- get_mermaid_endpoint("benthicattributes")
  expect_equal(names(output), mermaid_endpoint_columns[["benthicattributes"]])
  output <- get_mermaid_endpoint("fishattributes")
  expect_equal(names(output), mermaid_endpoint_columns[["fishattributes"]])
  output <- get_mermaid_endpoint("fishfamilies")
  expect_equal(names(output), mermaid_endpoint_columns[["fishfamilies"]])
  output <- get_mermaid_endpoint("fishgenera")
  expect_equal(names(output), mermaid_endpoint_columns[["fishgenera"]])
  output <- get_mermaid_endpoint("fishspecies")
  expect_equal(names(output), mermaid_endpoint_columns[["fishspecies"]])
  output <- get_mermaid_endpoint("managements")
  expect_equal(names(output), mermaid_endpoint_columns[["managements"]])
  output <- get_mermaid_endpoint("projects")
  expect_equal(names(output), mermaid_endpoint_columns[["projects"]])
  output <- get_mermaid_endpoint("sites")
  expect_equal(names(output), mermaid_endpoint_columns[["sites"]])
})
