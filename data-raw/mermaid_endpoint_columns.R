mermaid_endpoint_columns <- list(
  beltfishtransectmethods = c("id", "transect", "sample_event", "fishbelt_transect", "observers", "obs_belt_fishes", "created_on", "updated_on"),
  beltfishes = c("id", "transect", "created_on", "updated_on"),
  benthicattributes = c("id", "name", "status", "parent", "updated_on", "created_on"),
  benthiclittransectmethods = character(0),
  benthicpittransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_benthic_pits", "created_on", "updated_on"),
  benthicpits = c("id", "transect", "interval_size", "created_on", "updated_on"),
  choices = c("name", "data"),
  collectrecords = c("id", "project", "profile", "stage", "data", "validations", "created_on", "updated_on"),
  fishattributes = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "trophic_group", "trophic_level", "functional_group", "vulnerability", "created_on", "updated_on"),
  fishfamilies = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "created_on", "updated_on"),
  fishgenera = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "family", "created_on", "updated_on"),
  fishspecies = c("id", "name", "display_name", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "created_on", "updated_on"),
  habitatcomplexities = c("id", "transect", "interval_size", "created_on", "updated_on"),
  obsbenthiclits = character(0),
  obsbenthicpits = c("id", "data", "interval", "include", "notes", "benthicpit", "attribute", "growth_form", "created_on", "updated_on"),
  obshabitatcomplexities = c("id", "data", "interval", "include", "notes", "habitatcomplexity", "score", "created_on", "updated_on"),
  obstransectbeltfishs = c("id", "data", "size", "count", "include", "notes", "beltfish", "fish_attribute", "size_bin", "created_on", "updated_on"),
  managements_project = c("id", "name", "name_secondary", "project", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "project", "project_name", "rules", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  observers = c("id", "profile", "profile_name", "rank", "transectmethod", "created_on", "created_by"),
  projects = c("id", "name", "countries", "num_sites", "tags", "notes", "status", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "created_on", "updated_on"),
  project_profiles = c("id", "profile", "profile_name", "project", "is_collector", "is_admin", "role", "created_on", "updated_on"),
  sampleevents = c("id", "depth", "data", "sample_date", "sample_time", "notes", "created_by", "site", "management", "visibility", "current", "relative_depth", "tide", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "project", "latitude", "longitude", "country_name", "reef_type_name", "reef_zone_name", "exposure_name", "predecessor", "created_on", "updated_on")
)

usethis::use_data(mermaid_endpoint_columns, overwrite = TRUE)
