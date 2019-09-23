test_that("search_projects throws a message if exact_name is TRUE and it returns more than one project (but doesn't if exact_name is FALSE)", {
  expect_message(search_projects("mermaid lagoon", exact_name = TRUE))
  expect_silent(search_projects("mermaid lagoon"))
})

test_that("check_single_project returns a warning if more than one result is returned", {
  x <- data.frame(a = 1:2)
  expect_message(check_single_project(x, "test"))
  y <- data.frame(a = 1)
  expect_silent(check_single_project(y, "test"))
})
