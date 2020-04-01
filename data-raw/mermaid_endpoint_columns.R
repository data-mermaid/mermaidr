mermaid_endpoint_columns <- list(
  benthicattributes = c("id", "name", "status", "parent", "updated_on", "created_on"),
  choices = c("name", "data"),
  fishfamilies = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "created_on", "updated_on"),
  fishgenera = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "family", "created_on", "updated_on"),
  fishspecies = c("id", "name", "display_name", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "created_on", "updated_on"),
  fishsizes = c("id", "name", "val", "fish_bin_size", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "project", "rules", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  projects = c("id", "name", "countries", "num_sites", "tags", "notes", "status", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "created_on", "updated_on"),
  projecttags = c("id", "name", "slug", "description", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "project", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
)

mermaid_project_endpoint_columns <- list(
  beltfishtransectmethods = c("id", "transect", "sample_event", "fishbelt_transect", "observers", "obs_belt_fishes", "created_on", "updated_on"),
  beltfishes = c("id", "transect", "created_on", "updated_on"),
  benthiclittransectmethods = c("id", "transect", "sample_event", "benthic_transect", "observers", "obs_benthic_lits", "created_on", "updated_on"),
  benthicpittransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_benthic_pits", "created_on", "updated_on"),
  benthicpits = c("id", "transect", "interval_size", "created_on", "updated_on"),
  collectrecords = c("id", "profile", "stage", "data", "validations", "created_on", "updated_on"),
  fishbelttransects = c("id", "notes", "number", "len_surveyed", "reef_slope", "width", "size_bin", "sample_event", "created_on", "updated_on"),
  habitatcomplexities = c("id", "transect", "interval_size", "created_on", "updated_on"),
  obsbenthiclits = c("id", "length", "notes", "benthiclit", "attribute", "growth_form", "created_on", "updated_on"),
  obsbenthicpits = c("id", "data", "interval", "include", "notes", "benthicpit", "attribute", "growth_form", "created_on", "updated_on"),
  obshabitatcomplexities = c("id", "data", "interval", "include", "notes", "habitatcomplexity", "score", "created_on", "updated_on"),
  obstransectbeltfishs = c("id", "data", "size", "count", "include", "notes", "beltfish", "fish_attribute", "size_bin", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  observers = c("id", "profile", "rank", "transectmethod", "created_on", "created_by"),
  project_profiles = c("id", "profile", "is_collector", "is_admin", "role", "created_on", "updated_on"),
  sampleevents = c("id", "depth", "data", "sample_date", "sample_time", "notes", "created_by", "site", "management", "visibility", "current", "relative_depth", "tide", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on"),
  `beltfishes/obstransectbeltfishes` = c("tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_len_surveyed", "transect_width", "size_bin", "observers", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleunits` = c("tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "transect_number", "size_bin", "transect_len_surveyed", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleevents` = c("tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link")
)

usethis::use_data(mermaid_endpoint_columns, overwrite = TRUE)
usethis::use_data(mermaid_project_endpoint_columns, overwrite = TRUE)
