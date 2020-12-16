test_that("mermaid_search_my_projects only returns projects I have access to", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Indonesia")
  expect_true(nrow(output) == 3)
})
