test_that("blacklist method retains all columns from whitelist method,
          none from blacklist", {
  # Top level functions ----

  res <- mermaid_get_sites(limit = 5)
  expect_true(all(legacy_columns[["sites"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["sites"]]))
  expect_false(any(blacklist_columns[["site"]] %in% names(res)))

  res <- mermaid_get_projects(limit = 5)
  expect_true(all(legacy_columns[["projects"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["projects"]]))
  expect_false(any(blacklist_columns[["projects"]] %in% names(res)))

  res <- mermaid_get_managements(limit = 5)
  expect_true(all(legacy_columns[["managements"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["managements"]]))
  expect_false(any(blacklist_columns[["managements"]] %in% names(res)))

  res <- mermaid_get_me()
  expect_true(all(legacy_columns[["me"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["me"]]))
  expect_false(any(blacklist_columns[["me"]] %in% names(res)))

  ## project data top level functions ----

  # Testing that project identifier gets added when one project or multiple projects
  res <- mermaid_get_project_managements(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14"), limit = 5)
  expect_true(all(legacy_columns[["project_data"]][["managements"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["project_data"]][["managements"]]))
  expect_false(any(blacklist_columns[["project_data"]][["managements"]] %in% names(res)))

  res <- mermaid_get_project_managements(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "170e7182-700a-4814-8f1e-45ee1caf3b44"), limit = 5)
  expect_true(all(legacy_columns[["project_data"]][["managements"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["project_data"]][["managements"]]))
  expect_false(any(blacklist_columns[["project_data"]][["managements"]] %in% names(res)))

  res <- mermaid_get_project_sites(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14"), limit = 5)
  expect_true(all(legacy_columns[["project_data"]][["sites"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["project_data"]][["sites"]]))
  expect_false(any(blacklist_columns[["project_data"]][["sites"]] %in% names(res)))

  res <- mermaid_get_project_sites(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "170e7182-700a-4814-8f1e-45ee1caf3b44"), limit = 5)
  expect_true(all(legacy_columns[["project_data"]][["sites"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["project_data"]][["sites"]]))
  expect_false(any(blacklist_columns[["project_data"]][["sites"]] %in% names(res)))

  # mermaid_get_reference() ----

  res <- mermaid_get_reference("benthicattributes")
  expect_true(all(legacy_columns[["reference"]][["benthicattributes"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["benthicattributes"]]))
  expect_false(any(blacklist_columns[["reference"]][["benthicattributes"]] %in% names(res)))

  res <- mermaid_get_reference("fishfamilies")
  expect_true(all(legacy_columns[["reference"]][["fishfamilies"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["fishfamilies"]]))
  expect_false(any(blacklist_columns[["reference"]][["fishfamilies"]] %in% names(res)))

  res <- mermaid_get_reference("fishgenera")
  expect_true(all(legacy_columns[["reference"]][["fishgenera"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["fishgenera"]]))
  expect_false(any(blacklist_columns[["reference"]][["fishgenera"]] %in% names(res)))

  res <- mermaid_get_reference("fishspecies")
  expect_true(all(legacy_columns[["reference"]][["fishspecies"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["fishspecies"]]))
  expect_false(any(blacklist_columns[["reference"]][["fishspecies"]] %in% names(res)))

  res <- mermaid_get_reference("invertattributes")
  expect_true(all(legacy_columns[["reference"]][["invertattributes"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["invertattributes"]]))
  expect_false(any(blacklist_columns[["reference"]][["invertattributes"]] %in% names(res)))

  res <- mermaid_get_reference("invertspecies")
  expect_true(all(legacy_columns[["reference"]][["invertspecies"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["reference"]][["invertspecies"]]))
  expect_false(any(blacklist_columns[["reference"]][["invertspecies"]] %in% names(res)))

  # mermaid_get_endpoint() ----

  res <- mermaid_get_endpoint("choices")
  expect_true(all(legacy_columns[["endpoint"]][["choices"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["endpoint"]][["choices"]]))
  expect_false(any(blacklist_columns[["endpoint"]][["choices"]] %in% names(res)))

  res <- mermaid_get_endpoint("projecttags")
  expect_true(all(legacy_columns[["endpoint"]][["projecttags"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["endpoint"]][["projecttags"]]))
  expect_false(any(blacklist_columns[["endpoint"]][["projecttags"]] %in% names(res)))

  res <- mermaid_get_endpoint("fishsizes")
  expect_true(all(legacy_columns[["endpoint"]][["fishsizes"]] %in% names(res)))
  expect_true(all(names(res) %in% legacy_columns[["endpoint"]][["fishsizes"]]))
  expect_false(any(blacklist_columns[["endpoint"]][["fishsizes"]] %in% names(res)))
})
