#' Get MERMAID reference
#'
#' Find the names and information of the fish and benthic attributes you can choose in MERMAID.
#'
#' @param reference MERMAID reference. One of "fishfamilies", "fishgenera", "fishspecies", "benthicattributes".
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_reference("benthicattributes")
#' mermaid_get_reference(c("fishfamilies", "fishgenera"))
#' }
mermaid_get_reference <- function(reference = c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"), limit = NULL, url = base_url) {
  if (!all(reference %in% c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"))) {
    stop('`reference` must be one of: "fishfamilies", "fishgenera", "fishspecies", "benthicattributes"', call. = FALSE)
  }

  reference <- match.arg(reference, several.ok = TRUE)

  get_endpoint(reference, limit = limit, url = base_url)
}

fishfamilies_columns <- c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "created_on", "updated_on")
fishgenera_columns <- c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "family", "created_on", "updated_on")
fishspecies_columns <- c("id", "name", "display", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "created_on", "updated_on")
benthicattributes_columns <- c("id", "name", "status", "parent", "updated_on", "created_on")
