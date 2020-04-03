test_that("mermaid_get_endpoint throws an error when an unexpected endpoint is passed", {
  expect_error(mermaid_get_endpoint("ldkgjdgk"), "should be one of")
})

test_that("mermaid_get_endpoint returns a tibble when passed a known endpoint.", {
  skip_if_offline()
  expect_is(mermaid_get_endpoint("sites", limit = 1), "tbl_df")
})

test_that("mermaid_get_endpoint returns the correct columns given the endpoint.", {
  skip_if_offline()
  expect_named(mermaid_get_endpoint("benthicattributes", limit = 1), mermaid_endpoint_columns[["benthicattributes"]])
  expect_named(mermaid_get_endpoint("choices"), mermaid_endpoint_columns[["choices"]])
  expect_named(mermaid_get_endpoint("fishfamilies", limit = 1), mermaid_endpoint_columns[["fishfamilies"]])
  expect_named(mermaid_get_endpoint("fishgenera", limit = 1), mermaid_endpoint_columns[["fishgenera"]])
  expect_named(mermaid_get_endpoint("fishspecies", limit = 1), mermaid_endpoint_columns[["fishspecies"]])
  expect_named(mermaid_get_endpoint("fishsizes", limit = 1), mermaid_endpoint_columns[["fishsizes"]])
  expect_named(mermaid_get_endpoint("managements", limit = 1), mermaid_endpoint_columns[["managements"]])
  expect_named(mermaid_get_endpoint("projects", limit = 1), mermaid_endpoint_columns[["projects"]])
  expect_named(mermaid_get_endpoint("projecttags", limit = 1), mermaid_endpoint_columns[["projecttags"]])
  expect_named(mermaid_get_endpoint("sites", limit = 1), mermaid_endpoint_columns[["sites"]])
})

test_that("mermaid_get_endpoint can return for multiple endpoints", {
  output <- mermaid_get_endpoint(c("sites", "managements"), limit = 1)
  expect_is(output, "list")
  expect_named(output, c("sites", "managements"))
})
