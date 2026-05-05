test_that("blacklist method retains all columns from whitelist method,
          none from blacklist", {

  res <- mermaid_get_sites(limit = 10)
  expect_true(all(legacy_columns[["sites"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["sites"]]))
  expect_false(any(blacklist_columns[["site"]] %in% names(res)))

  res <- mermaid_get_reference("benthicattributes")
  expect_true(all(legacy_columns[["benthicattributes"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["benthicattributes"]]))
  expect_false(any(blacklist_columns[["benthicattributes"]] %in% names(res)))

  res <- mermaid_get_reference("fishfamilies")
  expect_true(all(legacy_columns[["fishfamilies"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["fishfamilies"]]))
  expect_false(any(blacklist_columns[["fishfamilies"]] %in% names(res)))

  res <- mermaid_get_reference("fishgenera")
  expect_true(all(legacy_columns[["fishgenera"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["fishgenera"]]))
  expect_false(any(blacklist_columns[["fishgenera"]] %in% names(res)))

  res <- mermaid_get_reference("fishspecies")
  expect_true(all(legacy_columns[["fishspecies"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["fishspecies"]]))
  expect_false(any(blacklist_columns[["fishspecies"]] %in% names(res)))

  res <- mermaid_get_projects(limit = 10)
  expect_true(all(legacy_columns[["projects"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["projects"]]))
  expect_false(any(blacklist_columns[["projects"]] %in% names(res)))

  res <- mermaid_get_managements(limit = 10)
  expect_true(all(legacy_columns[["managements"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["managements"]]))
  expect_false(any(blacklist_columns[["managements"]] %in% names(res)))

  res <- mermaid_get_me()
  expect_true(all(legacy_columns[["me"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["me"]]))
  expect_false(any(blacklist_columns[["me"]] %in% names(res)))
})
