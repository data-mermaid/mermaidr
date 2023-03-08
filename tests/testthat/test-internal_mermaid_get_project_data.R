test_that("fishbelt - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- c(
    "02e6915c-1c64-4d2c-bac0-326b560415a2", "2c0c9857-b11c-4b82-b7ef-e9b383d1233c",
    "2d6cee25-c0ff-4f6f-a8cd-667d3f2b914b", "a1b7ff1f-81cd-4efc-978b-cf0993f8d3c6"
  )
  old <- mermaid_get_project_data_legacy(p, method = "fishbelt", data = "all")
  new <- mermaid_get_project_data(p, method = "fishbelt", data = "all")

  old_obs <- old[["observations"]] %>%
    dplyr::arrange(project, site, management, management_secondary, management_parties, sample_date, sample_time, label, reef_slope, current, depth, relative_depth, transect_number, fish_family, fish_genus, fish_taxon, count) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    dplyr::select(-management_parties, -management_rules, -observers)

  new_obs <- new[["observations"]] %>%
    dplyr::arrange(project, site, management, management_secondary, management_parties, sample_date, sample_time, label, reef_slope, current, depth, relative_depth, transect_number, fish_family, fish_genus, fish_taxon, count) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    dplyr::select(-management_parties, -management_rules, -observers)

  expect_identical(old_obs, new_obs)

  old_su <- old[["sampleunits"]] %>%
    dplyr::select(tidyselect::all_of(names(new[["sampleunits"]]))) %>%
    dplyr::arrange(project, management, site, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope, transect_number) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    dplyr::select(-management_parties, -management_rules, -sample_unit_ids)

  new_su <- new[["sampleunits"]] %>%
    dplyr::arrange(project, management, site, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope, transect_number) %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    dplyr::select(-management_parties, -management_rules, -sample_unit_ids)

  expect_identical(old_su, new_su)

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::select(tidyselect::all_of(names(new[["sampleevents"]]))) %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::select(-management_parties, -management_rules),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::select(-management_parties, -management_rules)
  )
})


test_that("benthiclit - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "benthiclit", data = "all")
  new <- mermaid_get_project_data(p, method = "benthiclit", data = "all")

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules, -observers),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time) %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules, -observers)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules, -observers),
    new[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules, -observers)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::select(-management_parties, -management_rules)
  )
})

test_that("benthicpit - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- c(
    "2c0c9857-b11c-4b82-b7ef-e9b383d1233c", "5679ef3d-bafc-453d-9e1a-a4b282a8a997",
    "e1efb1e0-0af8-495a-9c69-fddcdba11c14"
  )
  old <- mermaid_get_project_data_legacy(p, method = "benthicpit", data = "all")
  new <- mermaid_get_project_data(p, method = "benthicpit", data = "all")

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("benthicpqt - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"
  old <- mermaid_get_project_data_legacy(p, method = "benthicpqt", data = "all")
  new <- mermaid_get_project_data(p, method = "benthicpqt", data = "all")

  expect_identical(
    old[["observations"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(-management_parties, -management_rules),
    new[["observations"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::select(-management_parties, -management_rules)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules),
    new[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::arrange(project, site, management_parties, sample_date, tide, current, visibility) %>%
      dplyr::select(-management_parties, -management_rules),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::arrange(project, site, management_parties, sample_date, tide, current, visibility) %>%
      dplyr::select(-management_parties, -management_rules)
  )
})

test_that("habitatcomplexity - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- mermaid_get_my_projects()
  old <- mermaid_get_project_data_legacy(p, method = "habitatcomplexity", data = "all")
  new <- mermaid_get_project_data(p, method = "habitatcomplexity", data = "all")

  expect_identical(
    old[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 3),
    new[["observations"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 3)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleunits"]] %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth, reef_slope) %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2),
    new[["sampleevents"]] %>%
      dplyr::select(-management_parties, -management_rules) %>%
      dplyr::mutate_if(is.numeric, round, 2)
  )
})

test_that("bleaching - new method of using CSV endpoint produces same data as old method (using JSON)", {
  p <- "d065cba4-ed09-47fd-89fb-2293fbbf617f"
  old <- mermaid_get_project_data_legacy(p, method = "bleaching", data = "all")
  new <- mermaid_get_project_data(p, method = "bleaching", data = "all")

  expect_identical(
    old[["observations"]][["colonies_bleached"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth) %>%
      dplyr::select(-management_parties, -observers),
    new[["observations"]][["colonies_bleached"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::arrange(project, site, management_parties, sample_date, sample_time, tide, current, visibility, relative_depth, depth) %>%
      dplyr::select(-management_parties, -observers)
  )

  expect_identical(
    old[["observations"]][["percent_cover"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(-management_parties, -observers),
    new[["observations"]][["percent_cover"]] %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::select(-management_parties, -observers)
  )

  expect_identical(
    old[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(-management_parties),
    new[["sampleunits"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, label, site_notes, management_notes, sample_unit_notes), dplyr::coalesce, "")) %>%
      dplyr::select(-management_parties)
  )

  expect_identical(
    old[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(-management_parties),
    new[["sampleevents"]] %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(dplyr::across(c(management_secondary, management_parties, site_notes, management_notes), dplyr::coalesce, "")) %>%
      dplyr::select(-management_parties)
  )
})

test_that("csv is faster than json", {})
