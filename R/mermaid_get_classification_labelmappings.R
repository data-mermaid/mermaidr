#' Classification label mappings
#'
#' Get a lookup table that maps MERMAID Benthic Attributes and Growth Forms to labels from other providers (e.g. CoralNet, ReefCloud).
#'
#' @inheritParams mermaid_GET
#' @param provider Provider to filter the lookup table for, e.g. CoralNet or ReefCloud.
#'
#' @export
#'
#' @examples
#' mermaid_get_classification_labelmappings()
mermaid_get_classification_labelmappings <- function(provider = NULL, limit = NULL) {
  endpoint <- "classification/labelmappings"
  if (!is.null(provider)) {
    get_endpoint(endpoint = endpoint, limit = limit, filter = list(provider = provider))
  } else {
    get_endpoint(endpoint = endpoint, limit = limit)
  }
}

classification_labelmappings_columns <- c("id", "benthic_attribute", "growth_form", "provider_id", "provider_label", "provider")
