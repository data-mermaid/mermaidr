#' Get or check field options for MERMAID import
#'
#' Check the options available for a given field in MERMAID, either to see what the options are or to check that your data matches the available fields.
#'
#' @param field Field to check options for. One of: width, fishsizebin, reefslope, visibility, current, relativedepth, tide, fishname, benthicattribute, growthform, habitatcomplexityscore.
#' @param values Optional values from your data, to match against valid options in Collect
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_import_field_options("growthform")
#'
#' mermaid_import_field_options("growthform", c("Arboresent", "Branching", "Columar", "Foliose"))
#' }
mermaid_import_field_options <- function(field = c("width", "fishsizebin", "reefslope", "visibility", "current", "relativedepth", "tide", "fishname", "benthicattribute", "growthform", "habitatcomplexityscore"), values) {
  field_options <- c("width", "fishsizebin", "reefslope", "visibility", "current", "relativedepth", "tide", "fishname", "benthicattribute", "growthform", "habitatcomplexityscore")

  if (!field %in% field_options) {
    stop("`field` must be one of: ", paste0(sort(field_options), collapse = ", "), call. = FALSE)
  }

  internal_field_name <- dplyr::case_when(
    field == "width" ~ "belttransectwidths",
    field == "fishname" ~ "fishspecies",
    field == "visibility" ~ "visibilities",
    TRUE ~ paste0(field, "s")
  )

  if (internal_field_name %in% c("belttransectwidths", "fishsizebins", "reefslopes", "visibilities", "currents", "relativedepths", "tides", "growthforms", "habitatcomplexityscores")) {
    choices <- mermaid_get_endpoint("choices")

    res <- choices %>%
      dplyr::filter(.data$name == internal_field_name) %>%
      dplyr::pull(.data$data) %>%
      purrr::pluck(1) %>%
      dplyr::select(mermaid_value = .data$name)
  } else if (internal_field_name == "fishspecies") {
    res <- get_endpoint(internal_field_name) %>%
      dplyr::select(value = .data$display)
  } else if (internal_field_name == "benthicattributes") {
    res <- get_endpoint(internal_field_name) %>%
      dplyr::select(mermaid_value = .data$name)
  }

  # If not checking any options, just return the possible values
  if (missing(values)) {
    return(res)
  }

  # Otherwise, check them!
  values_df <- dplyr::tibble(value = unique(values))

  values_df %>%
    fuzzyjoin::stringdist_left_join(res,
      by = c("value" = "mermaid_value"),
      distance_col = "diff", max_dist = Inf
    ) %>%
    dplyr::group_by(.data$value) %>%
    dplyr::filter(diff == min(.data$diff)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(match = .data$diff == 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$value, closest_mermaid_value = .data$mermaid_value, .data$match)
}
