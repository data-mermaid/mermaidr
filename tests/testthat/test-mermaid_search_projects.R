test_that("mermaid_search_projects throws a message if it returns more than one project (when searched by name only), and doesn't if it only returns one", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_message(mermaid_search_projects("test", include_test_projects = TRUE))
  expect_silent(mermaid_search_projects("Sharla test", include_test_projects = TRUE))
})

test_that("mermaid_search_projects returns a zero row tibble if the project is not found", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  name <- paste0(letters, collapse = "")
  output <- mermaid_search_projects(name = name)
  expect_equal(nrow(output), 0)
  expect_is(output, "tbl_df")
})

test_that("mermaid_search_projects returns a zero row tibble if a project is not found, and it has the same names as if a project was found", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
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
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_message(mermaid_search_projects("test", include_test_projects = TRUE), "More than one project with the name")
  expect_equal(unique(mermaid_search_projects("test", include_test_projects = TRUE)[["name"]]), "test")
  expect_equal(unique(mermaid_search_projects("2016_Macmon_Bua_Ra_Ovalau surveys")[["name"]]), "2016_Macmon_Bua_Ra_Ovalau surveys")
})

test_that("mermaid_search_projects searches by name, then filters by country if both are specified. There is no message if more than one with the name exists", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_silent(mermaid_search_projects(name = "test", countries = "Indonesia"))
  output <- mermaid_search_projects(name = "test", countries = "Indonesia", include_test_projects = TRUE)
  expect_equal(unique(output[["name"]]), "test")
  expect_true(all_contain_value(output[["countries"]], "Indonesia"))
  output <- mermaid_search_projects(name = "Test Project", countries = "Fiji", include_test_projects = TRUE)
  expect_equal(unique(output[["name"]]), "Test Project")
  expect_true(all_contain_value(output[["countries"]], "Fiji"))

  output <- mermaid_search_projects(name = "Test Project", countries = "Brazil")
  expect_true(nrow(output) == 0)
})

test_that("mermaid_search_projects searches by name, then filters by tag if both are specified.", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(name = "Test Project", tags = "Cytonn", include_test_projects = TRUE)
  expect_equal(unique(output[["name"]]), "Test Project")
  expect_true(all_contain_value(output[["tags"]], "Cytonn"))

  output <- mermaid_search_projects(name = "Test Project", tags = "Fiji", include_test_projects = TRUE)
  expect_true(nrow(output) == 0)
})

test_that("mermaid_search_projects returns projects that all have Country if Country filter is used", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(countries = "Fiji")
  expect_true(all_contain_value(output[["countries"]], "Fiji"))

  output <- mermaid_search_projects(countries = "Indonesia", token = mermaid_token())
  expect_true(nrow(output) > 0)
})

test_that("mermaid_search_projects returns projects that all have tag if Tag filter is used", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(tags = "WCS Fiji")
  expect_true(all_contain_value(output[["tags"]], "WCS Fiji"))
})

test_that("mermaid_search_projects errors if nothing is provided to search by", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_search_projects(limit = 1), "haven't provided")
})

test_that("mermaid_search_projects returns `countries` and `tags` that are character columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(countries = "Fiji")
  expect_is(output[["countries"]], "character")
  expect_is(output[["tags"]], "character")
})

test_that("mermaid_search_projects returns `countries` and `tags` that are semi-colon separated", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(countries = "Fiji")
  expect_true(any(grepl(";", output[["countries"]])))
  expect_true(any(grepl(";", output[["tags"]])))
})

test_that("mermaid_search_projects respects limit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_projects(countries = "Fiji", limit = 1)
  expect_true(nrow(output) == 1)
})

test_that("mermaid_search_projects returns same columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_named(mermaid_search_projects("Test", include_test_projects = TRUE), mermaid_endpoint_columns[["projects"]])
  expect_named(mermaid_search_projects(countries = "Fiji"), mermaid_endpoint_columns[["projects"]])
  expect_named(mermaid_search_projects(tags = "WCS Fiji"), mermaid_endpoint_columns[["projects"]])
})
