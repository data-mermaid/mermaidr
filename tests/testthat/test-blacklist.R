test_that("blacklist method retains all columns from whitelist method,
          none from blacklist", {

  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # Top level functions ----

  mermaid_get_sites(limit = 5) %>%
    check_columns("sites")

  mermaid_get_projects(limit = 5) %>%
    check_columns("projects")

  mermaid_get_my_projects(limit = 5) %>%
    check_columns("projects")

  mermaid_get_managements(limit = 5) %>%
    check_columns("managements")

  mermaid_get_me() %>%
    check_columns("me")

  mermaid_get_summary_sampleevents(limit = 5) %>%
    check_columns("summarysampleevents", all_cols_known = FALSE)

  mermaid_get_classification_labelmappings() %>%
    check_columns("classification/labelmappings")

  ## project data top level functions ----

  # Testing that project identifier gets added when one project or multiple projects
  mermaid_get_project_managements(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14"), limit = 5) %>%
    check_columns("managements", nested = "project_data")

  mermaid_get_project_managements(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "170e7182-700a-4814-8f1e-45ee1caf3b44"), limit = 5) %>%
    check_columns("managements", nested = "project_data")

  mermaid_get_project_sites(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14"), limit = 5) %>%
    check_columns("sites", nested = "project_data")

  mermaid_get_project_sites(c("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "170e7182-700a-4814-8f1e-45ee1caf3b44"), limit = 5) %>%
    check_columns("sites", nested = "project_data")

  # mermaid_get_reference() ----

  mermaid_get_reference("benthicattributes") %>%
    check_columns("benthicattributes", nested = "reference")

  mermaid_get_reference("fishfamilies") %>%
    check_columns("fishfamilies", nested = "reference")

  mermaid_get_reference("fishgenera") %>%
    check_columns("fishgenera", nested = "reference")

  mermaid_get_reference("fishspecies") %>%
    check_columns("fishspecies", nested = "reference")

  mermaid_get_reference("invertattributes") %>%
    check_columns("invertattributes", nested = "reference")

  mermaid_get_reference("invertspecies") %>%
    check_columns("invertspecies", nested = "reference")

  # mermaid_get_endpoint() ----

  mermaid_get_endpoint("choices") %>%
    check_columns("choices", nested = "endpoint")

  mermaid_get_endpoint("projecttags") %>%
    check_columns("projecttags", nested = "endpoint")

  mermaid_get_endpoint("fishsizes") %>%
    check_columns("fishsizes", nested = "endpoint")

  # Project data functions ---

  ## Fishbelt ----
  p <- "e1efb1e0-0af8-495a-9c69-fddcdba11c14"
  mermaid_get_project_data(p, "fishbelt", "observations") %>%
    check_columns("beltfishes/obstransectbeltfishes", nested = "project_data")

  mermaid_get_project_data(p, "fishbelt", "sampleunits") %>%
    check_columns("beltfishes/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "fishbelt", "sampleevents") %>%
    check_columns("beltfishes/sampleevents", nested = "project_data")

  ## Benthic PIT ----
  mermaid_get_project_data(p, "benthicpit", "observations") %>%
    check_columns("benthicpits/obstransectbenthicpits", nested = "project_data")

  mermaid_get_project_data(p, "benthicpit", "sampleunits") %>%
    check_columns("benthicpits/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "benthicpit", "sampleevents") %>%
    check_columns("benthicpits/sampleevents", nested = "project_data")

  ## Benthic LIT ----
  mermaid_get_project_data(p, "benthiclit", "observations") %>%
    check_columns("benthiclits/obstransectbenthiclits", nested = "project_data")

  mermaid_get_project_data(p, "benthiclit", "sampleunits") %>%
    check_columns("benthiclits/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "benthiclit", "sampleevents") %>%
    check_columns("benthiclits/sampleevents", nested = "project_data")

  ## Benthic PQT ----
  p <- "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"

  mermaid_get_project_data(p, "benthicpqt", "observations") %>%
    check_columns("benthicpqts/obstransectbenthicpqts", nested = "project_data")

  mermaid_get_project_data(p, "benthicpqt", "sampleunits") %>%
    check_columns("benthicpqts/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "benthicpqt", "sampleevents") %>%
    check_columns("benthicpqts/sampleevents", nested = "project_data")

  ## Habitat Complexity ----
  mermaid_get_project_data(p, "habitatcomplexity", "observations") %>%
    check_columns("habitatcomplexities/obshabitatcomplexities", nested = "project_data")

  mermaid_get_project_data(p, "habitatcomplexity", "sampleunits") %>%
    check_columns("habitatcomplexities/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "habitatcomplexity", "sampleevents") %>%
    check_columns("habitatcomplexities/sampleevents", nested = "project_data")

  ## Bleaching ----
  obs <- mermaid_get_project_data(p, "bleaching", "observations")

  obs[["colonies_bleached"]] %>%
    check_columns("bleachingqcs/obscoloniesbleacheds", nested = "project_data")

  obs[["percent_cover"]] %>%
    check_columns("bleachingqcs/obsquadratbenthicpercents", nested = "project_data")

  mermaid_get_project_data(p, "bleaching", "sampleunits") %>%
    check_columns("bleachingqcs/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "bleaching", "sampleevents") %>%
    check_columns("bleachingqcs/sampleevents", nested = "project_data")

  ## Inverts ----
  p <- "bacd3529-e0f4-40f4-a089-992c5bd5cc02"

  mermaid_get_project_data(p, "macroinvertebrate", "observations") %>%
    check_columns("beltinverts/obstransectbeltinverts", nested = "project_data")

  mermaid_get_project_data(p, "macroinvertebrate", "sampleunits") %>%
    check_columns("beltinverts/sampleunits", nested = "project_data")

  mermaid_get_project_data(p, "macroinvertebrate", "sampleevents") %>%
    check_columns("beltinverts/sampleevents", nested = "project_data")

  ## Other project endpoints -----

  mermaid_get_project_endpoint(p, "beltfishtransectmethods") %>%
    check_columns("beltfishtransectmethods", nested = "project_data")

  # Checking 1 and multiple projects
  mermaid_get_project_endpoint(c(p, "2c0c9857-b11c-4b82-b7ef-e9b383d1233c"), "beltfishtransectmethods") %>%
    check_columns("beltfishtransectmethods", nested = "project_data")

  mermaid_get_project_endpoint(p, "benthiclittransectmethods") %>%
    check_columns("benthiclittransectmethods", nested = "project_data")

  mermaid_get_project_endpoint(p, "benthicpittransectmethods") %>%
    check_columns("benthicpittransectmethods", nested = "project_data")

  mermaid_get_project_endpoint(p, "collectrecords") %>%
    check_columns("collectrecords", nested = "project_data")

  mermaid_get_project_endpoint(p, "fishbelttransects") %>%
    check_columns("fishbelttransects", nested = "project_data")

  mermaid_get_project_endpoint(p, "observers") %>%
    check_columns("observers", nested = "project_data")

  mermaid_get_project_endpoint(p, "project_profiles") %>%
    check_columns("project_profiles", nested = "project_data")

  mermaid_get_project_endpoint(p, "sampleevents") %>%
    check_columns("sampleevents", nested = "project_data")
})
