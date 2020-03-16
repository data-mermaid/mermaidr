test_that("mermaid_list_projects returns a list of projects depending on limit", {
  output <- mermaid_list_projects(1)
  expect_true(nrow(output) == 1)
})

