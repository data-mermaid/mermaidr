#' Classification label mappings
#'
#' Get a lookup table that maps MERMAID Benthic Attributes and Growth Forms to labels from other providers (CoralNet and ReefCloud).
#'
#' @inheritParams mermaid_GET
#' @param provider Provider to filter the lookup table for. Allowed options are CoralNet and ReefCloud.
#'
#' @export
#'
#' @examples
#' mermaid_get_classification_labelmappings()
mermaid_get_classification_labelmappings <- function(provider = NULL, limit = NULL) {
  endpoint <- "classification/labelmappings"

  if (!is.null(provider)) {
    # Error if the provider is not CoralNet or ReefCloud
    if (length(provider) != 1 | !all(provider %in% allowed_providers)) {
      stop(glue::glue('`provider` must be one of: "{providers}"', providers = glue::glue_collapse(allowed_providers, sep = '", "')), call. = FALSE)
    }
    get_endpoint(endpoint = endpoint, limit = limit, filter = list(provider = provider))
  } else {
    get_endpoint(endpoint = endpoint, limit = limit)
  }
}

classification_labelmappings_columns <- c("id", "benthic_attribute", "growth_form", "provider_id", "provider_label", "provider")

allowed_providers <- c("CoralNet", "ReefCloud")
