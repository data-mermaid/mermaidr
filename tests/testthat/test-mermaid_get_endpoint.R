test_that("mermaid_get_endpoint throws an error when an unexpected endpoint is passed", {
  expect_error(mermaid_get_endpoint("ldkgjdgk"), "should be one of")
})

test_that("mermaid_get_endpoint returns a tibble when passed a known endpoint.", {
  skip_if_offline()
  expect_is(mermaid_get_endpoint("sites"), "tbl_df")
})

test_that("mermaid_get_endpoint returns the correct columns given the endpoint.", {
  skip_if_offline()
  output <- mermaid_get_endpoint("benthicattributes")
  expect_equal(names(output), mermaid_endpoint_columns[["benthicattributes"]])
  # output <- mermaid_get_endpoint("fishattributes") 500 error
  # expect_equal(names(output), mermaid_endpoint_columns[["fishattributes"]])
  output <- mermaid_get_endpoint("fishfamilies")
  expect_equal(names(output), mermaid_endpoint_columns[["fishfamilies"]])
  output <- mermaid_get_endpoint("fishgenera")
  expect_equal(names(output), mermaid_endpoint_columns[["fishgenera"]])
  output <- mermaid_get_endpoint("fishspecies")
  expect_equal(names(output), mermaid_endpoint_columns[["fishspecies"]])
  output <- mermaid_get_endpoint("managements")
  expect_equal(names(output), mermaid_endpoint_columns[["managements"]])
  output <- mermaid_get_endpoint("projects")
  expect_equal(names(output), mermaid_endpoint_columns[["projects"]])
  output <- mermaid_get_endpoint("sites")
  expect_equal(names(output), mermaid_endpoint_columns[["sites"]])
})
