test_that("non-numeric year results in error", {
  data <- tibble::tibble(
    name = "Test non-numeric year", name_secondary = "", notes = NA,
    est_year = "2018", size = NA, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "full",
    parties = "government"
  )

  expect_error(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "`est_year` must be numeric"
  )
})

test_that("non-numeric size results in error", {
  # Non-numeric size
  data <- tibble::tibble(
    name = "Test non-numeric size", name_secondary = "", notes = NA,
    est_year = 2018, size = "18ha", no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "full",
    parties = "government"
  )

  expect_error(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "`size` must be numeric"
  )
})


test_that("missing size is fine", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no size", name_secondary = "", notes = NA,
    est_year = 2018, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "full",
    parties = "government"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})


test_that("missing year is fine", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no year", name_secondary = "", notes = NA,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "full",
    parties = "government"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})


test_that("missing compliance is fine", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no compliance", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE,
    parties = "government"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})

test_that("missing parties is fine", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no parties", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "none"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})


test_that("missing parties and compliance is fine", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no parties or compliance", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})


test_that("invalid compliance errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test invalid compliance", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "noneee"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Not all values of `compliance` are valid. Invalid values: noneee"
  )
})

test_that("invalid parties (single) errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test invalid party", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "ngooo"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Not all values of `parties` are valid. Invalid values: 'ngooo'"
  )
})



test_that("invalid parties (one of multiple) errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test invalid party, one of many", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "NGO;gov"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Not all values of `parties` are valid. Invalid values: 'gov'"
  )
})



test_that("invalid parties (all) errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test invalid party, all invalid", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "private;gov"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Not all values of `parties` are valid. Invalid values: 'private', 'gov'"
  )
})

test_that("parties and compliance are case insensitive", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test case insensitive parties", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "NGO; Private Sector"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )

  data <- tibble::tibble(
    name = "Test case insensitive compliance", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = "Full"
  )

  expect_message(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported"
  )
})

test_that("separating by non ; doesn't work", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test improper sep", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "NGO,private sector"
  )

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Not all values of `parties` are valid. Invalid values: 'ngo,private sector'.")
})

test_that("separating by ';' and '; ' work", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test ; ", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "NGO ; private sector"
  )

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported")

  data <- tibble::tibble(
    name = "Test ;", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = "NGO;private sector"
  )

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported")
})

test_that("NA parties or compliance do not trigger errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test NA parties is fine", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, parties = NA
  )

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported")

  data <- tibble::tibble(
    name = "Test NA compliance is fine", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, no_take = FALSE, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = NA
  )

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported")
})

test_that("excluding rules is fine, gets treated as FALSE", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()


  # Missing any rules
  data <- tibble::tibble(
    name = "Test missing rule", name_secondary = "", notes = NA, est_year = 2018,
    size = 5, periodic_closure = TRUE,
    open_access = FALSE, size_limits = TRUE, gear_restriction = TRUE,
    species_restriction = FALSE, access_restriction = TRUE, compliance = NA
  )
  # Anything missing gets treated as FALSE

  expect_message(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "All managements imported")

  # TODO: read back in and confirm it's FALSE
})

test_that("missing all rules errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test missing all rules"
  )
  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "data must contain at least one of the rules:")

  data <- tibble::tibble(
    name = "Test has FALSE rules", open_access = FALSE
  )
  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "data must contain at least one of the rules:")

  data <- tibble::tibble(
    name = c("Test has FALSE rules", "Test has a TRUE rule"), open_access = c(F, T)
  )
  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "data must contain at least one of the rules:")
})

test_that("open access AND no take, a conflict, errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test open access AND no take", open_access = TRUE, no_take = TRUE
  )

  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Cannot have both `open_access` and `no_take` as TRUE.")
})

test_that("open access AND partial restrictions, a conflict, errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test open access AND partial", open_access = TRUE, gear_restriction = TRUE, size_limits = FALSE
  )

  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Cannot have both `open_access` and any partial restrictions rules")
})

test_that("no take AND partial restrictions, a conflict, errors", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  data <- tibble::tibble(
    name = "Test no take AND partial", no_take = TRUE, gear_restriction = TRUE
  )

  expect_error(mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "Cannot have both `no_take` and any partial restrictions rules")
})

test_that("project col that does not match project listed produces error", {
  data <- tibble::tibble(
    name = "Test wrong project", project = "test"
  )

  expect_error(
    mermaid_import_project_managements(data, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"),
    "`project` column does not match project being imported into."
  )
})
