#' Get MERMAID Endpoint
#'
#' @inheritParams mermaid_GET
#'
#' @export
get_mermaid_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishattributes", "fishfamilies", "fishgenera", "fishspecies", "managements", "projects", "sites"), limit = 50, url = base_url, ...) {
  endpoint <- check_endpoint(endpoint, mermaid_endpoint_columns)
  res <- mermaid_GET(endpoint, limit = limit, url = url, ...)

  if (nrow(res) == 0) {
    cols <- mermaid_endpoint_columns[[endpoint]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res[, mermaid_endpoint_columns[[endpoint]]]
  }
}

mermaid_endpoint_columns <- list(
  benthicattributes = c("id", "name", "status", "parent", "updated_on", "created_on"),
  choices = c("name", "data"),
  fishattributes = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "trophic_group", "trophic_level", "functional_group", "vulnerability", "created_on", "updated_on"),
  fishfamilies = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "created_on", "updated_on"),
  fishgenera = c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "family", "created_on", "updated_on"),
  fishspecies = c("id", "name", "display_name", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "project", "project_name", "rules", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  projects = c("id", "name", "countries", "num_sites", "tags", "notes", "status", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "project", "location", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on")
)
