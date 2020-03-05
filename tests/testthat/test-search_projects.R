test_that("search_projects throws a message if it returns more than one project, and doesn't if it only returns one", {
  expect_message(search_projects("test"))
  expect_silent(search_projects("Sharla test"))
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

test_that("search_projects returns a zero row tibble if a project is not found, and it has the same names as if a project was found", {
  set.seed(1234)
  id <- paste0(sample(1:1000, 3), collapse = "")
  output <- search_projects(id = id)

  output_exists <- search_projects("test")
  expect_equal(names(output), names(output_exists))
})

test_that("check_single_project returns a warning if more than one result is returned", {
  x <- data.frame(a = 1:2)
  expect_message(check_single_project(x, "test"))
  y <- data.frame(a = 1)
  expect_silent(check_single_project(y, "test"))
})
