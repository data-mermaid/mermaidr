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
mermaid_get_reference <- function(reference = c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"), limit = NULL) {
  if (!all(reference %in% c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"))) {
    stop('`reference` must be one of: "fishfamilies", "fishgenera", "fishspecies", "benthicattributes"', call. = FALSE)
  }

  reference <- match.arg(reference, several.ok = TRUE)

  res <- purrr::map(reference, get_single_reference, limit)

  if (length(reference) > 1) {
    names(res) <- reference
    res
  } else {
    res[[1]]
  }
}

get_single_reference <- function(reference, limit = NULL) {
  switch(reference,
    fishfamilies = get_endpoint("fishfamilies", limit = limit),
    fishgenera = get_reference_fishgenera(limit = limit),
    fishspecies = get_reference_fishspecies(limit = limit),
    benthicattributes = get_reference_benthicattributes(limit = limit)
  )
}

get_reference_fishgenera <- function(limit = NULL) {
  fishgenera <- get_endpoint("fishgenera", limit = limit)
  fishfamilies <- get_endpoint("fishfamilies") %>%
    dplyr::select(.data$id, family = .data$name)

  fishgenera %>%
    dplyr::left_join(fishfamilies, by = c("family" = "id"), suffix = c("_id", ""))
}

get_reference_fishspecies <- function(limit = NULL) {
  fishspecies <- get_endpoint("fishspecies", limit = limit)

  fishgenera <- get_endpoint("fishgenera")

  choices <- mermaid_get_endpoint("choices") %>%
    tibble::deframe()

  fishgroupsizes <- choices[["fishgroupsizes"]] %>%
    dplyr::select(.data$id, group_size = .data$name)

  fishgrouptrophics <- choices[["fishgrouptrophics"]] %>%
    dplyr::select(.data$id, trophic_group = .data$name)

  fishgroupfunctions <- choices[["fishgroupfunctions"]] %>%
    dplyr::select(.data$id, functional_group = .data$name)

  genus <- fishgenera %>%
    dplyr::select(.data$id, genus = .data$name)

  fishspecies %>%
    dplyr::rename(species = .data$display) %>%
    dplyr::left_join(genus, by = c("genus" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgroupsizes, by = c("group_size" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgrouptrophics, by = c("trophic_group" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgroupfunctions, by = c("functional_group" = "id"), suffix = c("_id", ""))
}

get_reference_benthicattributes <- function(limit = NULL) {
  benthicattributes <- get_endpoint("benthicattributes", limit = limit)

  benthicattributes %>%
    dplyr::left_join(benthicattributes %>%
      dplyr::select(parent_id = .data$id, parent = .data$name), by = c("parent" = "parent_id"), suffix = c("_id", ""))
}

fishfamilies_columns <- c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "created_on", "updated_on")
fishgenera_columns <- c("id", "name", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "family", "created_on", "updated_on")
fishspecies_columns <- c("id", "name", "display", "notes", "status", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "climate_score", "vulnerability", "max_length", "trophic_level", "max_length_type", "genus", "group_size", "trophic_group", "functional_group", "created_on", "updated_on")
benthicattributes_columns <- c("id", "name", "status", "parent", "updated_on", "created_on")
