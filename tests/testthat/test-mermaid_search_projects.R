test_that("mermaid_search_projects throws a message if it returns more than one project (when searched by name only), and doesn't if it only returns one", {
  expect_message(mermaid_search_projects("test"))
  expect_silent(mermaid_search_projects("Sharla test"))
})

test_that("mermaid_search_projects returns a zero row tibble if the project is not found", {
  name <- paste0(letters, collapse = "")
  output <- mermaid_search_projects(name = name)
  expect_equal(nrow(output), 0)
  expect_is(output, "tbl_df")
})

test_that("mermaid_search_projects returns a zero row tibble if a project is not found, and it has the same names as if a project was found", {
  set.seed(1234)
  id <- paste0(sample(1:1000, 3), collapse = "")
  output <- mermaid_search_projects(name = id)
  output_exists <- mermaid_search_projects("test")
  expect_equal(names(output), names(output_exists))
})

test_that("check_single_project returns a warning if more than one result is returned", {
  x <- data.frame(a = 1:2)
  expect_message(check_single_project(x, "test"))
  y <- data.frame(a = 1)
  expect_silent(check_single_project(y, "test"))
})

test_that("mermaid_search_projects returns projects with matching name `name` is used for searching. If there is more than one, there's a message", {
  expect_message(mermaid_search_projects("test"), "More than one project with the name")
  expect_equal(unique(mermaid_search_projects("test")[["name"]]), "test")
  expect_equal(unique(mermaid_search_projects("2016_Macmon_Bua_Ra_Ovalau surveys")[["name"]]), "2016_Macmon_Bua_Ra_Ovalau surveys")
})

test_that("mermaid_search_projects searches by name, then filters by country if both are specified. There is no message if more than one with the name exists", {
  expect_silent(mermaid_search_projects(name = "test", country = "Indonesia"))
  output <- mermaid_search_projects(name = "test", country = "Indonesia")
  expect_equal(unique(output[["name"]]), "test")
  expect_true(all_contain_value(output[["countries"]], "Indonesia"))
  output <- mermaid_search_projects(name = "Test Project", country = "Fiji")
  expect_equal(unique(output[["name"]]), "Test Project")
  expect_true(all_contain_value(output[["countries"]], "Fiji"))

  output <- mermaid_search_projects(name = "Test Project", country = "Brazil")
  expect_true(nrow(output) == 0)
})

test_that("mermaid_search_projects searches by name, then filters by tag if both are specified." , {
  output <- mermaid_search_projects(name = "Test Project", tag = "Cytonn")
  expect_equal(unique(output[["name"]]), "Test Project")
  expect_true(all_contain_value(output[["tags"]], "Cytonn"))

  output <- mermaid_search_projects(name = "Test Project", tag = "Fiji")
  expect_true(nrow(output) == 0)
})

test_that("mermaid_search_projects returns projects that all have Country if Country filter is used", {
  output <- mermaid_search_projects(country = "Fiji")
  expect_true(all_contain_value(output[["countries"]], "Fiji"))
})

test_that("mermaid_search_projects returns projects that all have tag if Tag filter is used", {
  output <- mermaid_search_projects(tag = "WCS Fiji")
  expect_true(all_contain_value(output[["tags"]], "WCS Fiji"))
})

test_that("mermaid_search_projects just returns `limit` projects if nothing is provided to search by, and returns a warning", {
  expect_warning(mermaid_search_projects(limit = 1))
  expect_true(nrow(mermaid_search_projects(limit = 1)) == 1)
})
