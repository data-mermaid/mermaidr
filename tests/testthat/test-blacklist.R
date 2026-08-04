test_that("blacklist method retains all columns from whitelist method,
          none from blacklist", {
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
  mermaid_get_project_data("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "fishbelt", "observations") %>%
    check_columns("beltfishes/obstransectbeltfishes", nested = "project_data")

  mermaid_get_project_data("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "fishbelt", "sampleunits") %>%
    check_columns("beltfishes/sampleunits", nested = "project_data")

  mermaid_get_project_data("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "fishbelt", "sampleevents") %>%
    check_columns("beltfishes/sampleevents", nested = "project_data")

  ## Benthic PIT ----
  mermaid_get_project_data("e1efb1e0-0af8-495a-9c69-fddcdba11c14", "benthicpit", "observations") %>%
    check_columns("benthicpits/obstransectbenthicpits", nested = "project_data")
})
