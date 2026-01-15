test_that("blacklist method retains all columns from whitelist method,
          none from blacklist", {

  res <- mermaid_get_sites(limit = 10)
  expect_true(all(legacy_columns[["sites"]] %in% names(res)))
  expect_false(any(blacklist_columns[["site"]] %in% names(res)))

  res <- mermaid_get_reference("benthicattributes")
  expect_true(all(legacy_columns[["benthicattributes"]] %in% names(res)))
  expect_false(any(blacklist_columns[["benthicattributes"]] %in% names(res)))

  res <- mermaid_get_projects(limit = 10)
  expect_true(all(legacy_columns[["projects"]] %in% names(res)))
  expect_false(any(blacklist_columns[["projects"]] %in% names(res)))

  res <- mermaid_get_managements(limit = 10)
  expect_true(all(legacy_columns[["managements"]] %in% names(res)))
  expect_false(any(blacklist_columns[["managements"]] %in% names(res)))
})
