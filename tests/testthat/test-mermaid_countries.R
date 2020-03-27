test_that("mermaid_countries returns a vector", {
  skip_if_offline()
  expect_is(mermaid_countries(), "character")
})
