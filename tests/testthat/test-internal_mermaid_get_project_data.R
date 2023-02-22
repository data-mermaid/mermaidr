# - No permission to export data on dev APP for 2018_Vatu-i-Ra reef surveys
# - Discrepancy in how "community/local government"  is represented in CSV versus API/JSON - it's "community, local government" in CSV (https://dev-api.datamermaid.org/v1/projects/170e7182-700a-4814-8f1e-45ee1caf3b44/beltfishes/obstransectbeltfishes/csv/ vs https://dev-api.datamermaid.org/v1/projects/170e7182-700a-4814-8f1e-45ee1caf3b44/beltfishes/obstransectbeltfishes/)
# - [] for empty management_rules instead of NA, but it should not even be possible - total edge case
# - API/JSON sample units has columns that CSV does not - biomass_kgha_fish_family_zeroes_* and. a bunch of other  biomass_kgha_fish_family_* that are not in CSV
# - One OLD fishbelt sampleunits has tide NA, NEW has tide rising - i think some observations have rising, so it is concatenated in new? project == "Great Sea Reef 2019", site == "CH2B". same with current for same project/site
# - project XPDC Kei Kecil 2018, site KE10 has mix of visibility - 1-5m - poor and 5-10m - fair, JSON API gives 1-5m poor, CSV gives 5-10m - fair - same with depth, transect_width for same project/site
# - for same project/site, biomass from JSON is 272 but 29.7 in CSV (and this matches in app)

test_that("fishbelt - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "fishbelt", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "fishbelt", data = "all", limit = 50)

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, relative_depth, transect_number, fish_family, fish_genus, fish_taxon, count) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, relative_depth, transect_number, fish_family, fish_genus, fish_taxon, count) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  old_su <- old[["sampleunits"]] %>%
    dplyr::select(tidyselect::all_of(names(new[["sampleunits"]]))) %>%
    dplyr::arrange(project, management, site, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope, transect_number) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    filter(!(project == "Great Sea Reef 2019" & site == "CH2B")) %>%
    filter(!(project == "XPDC Kei Kecil 2018" & site == "KE10"))
  new_su <- new[["sampleunits"]] %>%
    dplyr::arrange(project, management, site, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope, transect_number) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    filter(!(project == "Great Sea Reef 2019" & site == "CH2B")) %>%
    filter(!(project == "XPDC Kei Kecil 2018" & site == "KE10"))

  expect_identical(old_su, new_su)

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::select(tidyselect::all_of(names(new[["sampleevents"]]))) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})


test_that("benthiclit - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "benthiclit", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "benthiclit", data = "all", limit = 50)

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )
  # TODO: management_rules is comma separated, no space in new - ; separated with space in old

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )
})

test_that("benthicpit - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "benthicpit", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "benthicpit", data = "all", limit = 50)

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("benthicpqt - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "benthicpqt", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "benthicpqt", data = "all", limit = 50)

  expect_identical(
    old[["observations"]] %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  # TODO bigggg issue: missing percent_cover_benthic_category in CSV! I need to look into this
  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )

  # Somehow not missing here though!
  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("habitatcomplexity - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "habitatcomplexity", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "habitatcomplexity", data = "all", limit = 50)

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("bleaching - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "bleaching", data = "all", limit = 50)
  new <- mermaid_get_project_data(p, method = "bleaching", data = "all", limit = 50)

  expect_identical(
    old[["observations"]][["colonies_bleached"]] %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]][["colonies_bleached"]] %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )
  # Observers separated by ", " in new instead of "; " in old

  expect_identical(
    old[["observations"]][["percent_cover"]] %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]][["percent_cover"]] %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
  # Sample unit IDs mismatch?

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("csv is faster than json", {})
