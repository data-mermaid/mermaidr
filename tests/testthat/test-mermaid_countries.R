test_that("mermaid_countries returns a vector", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_is(mermaid_countries(), "character")
})
