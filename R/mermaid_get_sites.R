#' Get MERMAID sites
#'
#' Get all MERMAID sites (not project specific). Returns metadata on sites, including site ID and name, notes, project, latitude and longitude, country, reef type and zone, exposure, and when the site was created and last updated.
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_sites(limit = 10)
#' }
mermaid_get_sites <- function(limit = NULL, token = mermaid_token()) {
  res <- get_endpoint("sites", limit = limit, token = token)

  res %>%
    unpack_coordinates()
}

unpack_coordinates <- function(x) {
  if (nrow(x) > 0) {
    x %>%
      tidyr::unpack(cols = "location") %>%
      tidyr::hoist(.data$coordinates,
        latitude = 2,
        longitude = 1
      ) %>%
      dplyr::select(-dplyr::all_of(c("type", "coordinates")))
  } else {
    x
  }
}
