#' Classification label mappings
#'
#' Get a lookup table that maps MERMAID Benthic Attributes and Growth Forms to labels from other sources (e.g. CoralNet).
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' mermaid_get_classification_labelmappings()
mermaid_get_classification_labelmappings <- function(limit = NULL) {
  get_endpoint(endpoint = "classification/labelmappings", limit = limit)
}

classification_labelmappings_columns <- c("id", "benthic_attribute", "growth_form", "provider_id", "provider_label", "provider")
