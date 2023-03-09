
test_that("mermaid_search_my_projects only returns projects I have access to", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Indonesia")
  expect_true(nrow(output) == 7)
})

test_that("mermaid_search_my_projects throws a message if it returns more than one project (when searched by name only), and doesn't if it only returns one", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  # expect_message(mermaid_search_my_projects("test", include_test_projects = TRUE)) # Unique project names are enforced now, so test will fail - but keeping the warning in just in case it changes back
  expect_silent(mermaid_search_my_projects("Sharla test", include_test_projects = TRUE))
})

test_that("mermaid_search_my_projects returns a zero row tibble if the project is not found", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  name <- paste0(letters, collapse = "")
  output <- mermaid_search_my_projects(name = name)
  expect_equal(nrow(output), 0)
  expect_is(output, "tbl_df")
})

test_that("mermaid_search_my_projects returns a zero row tibble if a project is not found, and it has the same names as if a project was found", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  set.seed(1234)
  id <- paste0(sample(1:1000, 3), collapse = "")
  output <- mermaid_search_my_projects(name = id)
  output_exists <- mermaid_search_my_projects("test")
  expect_equal(names(output), names(output_exists))
})

test_that("check_single_project returns a warning if more than one result is returned", {
  x <- data.frame(a = 1:2)
  expect_message(check_single_project(x, "test"))
  y <- data.frame(a = 1)
  expect_silent(check_single_project(y, "test"))
})

test_that("mermaid_search_my_projects returns projects that all have Country if Country filter is used", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Fiji")
  expect_true(all_contain_value(output[["countries"]], "Fiji"))
})

test_that("mermaid_search_my_projects returns projects that all have tag if Tag filter is used", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(tags = "WCS Fiji")
  expect_true(all_contain_value(output[["tags"]], "WCS Fiji"))
})

test_that("mermaid_search_my_projects errors if nothing is provided to search by", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_error(mermaid_search_my_projects(limit = 1), "haven't provided")
})

test_that("mermaid_search_my_projects returns `countries` and `tags` that are character columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Fiji")
  expect_is(output[["countries"]], "character")
  expect_is(output[["tags"]], "character")
})

test_that("mermaid_search_my_projects returns `tags` that are semi-colon separated", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Indonesia")
  expect_true(any(grepl(";", output[["tags"]])))
})

test_that("mermaid_search_my_projects respects limit", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  output <- mermaid_search_my_projects(countries = "Fiji", limit = 1)
  expect_true(nrow(output) == 1)
})

test_that("mermaid_search_my_projects returns same columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  expect_named(mermaid_search_my_projects("Test", include_test_projects = TRUE), mermaid_endpoint_columns[["projects"]])
  expect_named(mermaid_search_my_projects(countries = "Fiji"), mermaid_endpoint_columns[["projects"]])
  expect_named(mermaid_search_my_projects(tags = "WCS Fiji"), mermaid_endpoint_columns[["projects"]])
})
