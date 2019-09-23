test_that("search_projects throws a message if exact_name is TRUE and it returns more than one project (but doesn't if exact_name is FALSE)", {
  expect_message(search_projects("mermaid lagoon", exact_name = TRUE))
  expect_silent(search_projects("mermaid lagoon"))
})

test_that("search_projects returns a zero row tibble if the project is not found", {
  name <- paste0(letters, collapse = "")
  output <- search_projects(name = name)
  expect_equal(nrow(output), 0)
  expect_is(output, "tbl_df")

  set.seed(1234)
  id <- paste0(sample(1:1000, 3), collapse = "")
  output <- search_projects(id = id)
  expect_equal(nrow(output), 0)
  expect_is(output, "tbl_df")
})

test_that("check_single_project returns a warning if more than one result is returned", {
  x <- data.frame(a = 1:2)
  expect_message(check_single_project(x, "test"))
  y <- data.frame(a = 1)
  expect_silent(check_single_project(y, "test"))
})
