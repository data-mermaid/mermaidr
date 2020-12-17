#' MERMAID countries
#'
#' Returns the names of countries in MERMAID to help with searching.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_countries()
#' }
mermaid_countries <- function() {
  check_internet()
  choices <- get_endpoint("choices", limit = NULL)
  dplyr::filter(choices, .data$name == "countries")[["data"]][[1]][["name"]]
}
