#' MERMAID countries
#'
#' Returns the names of countries in MERMAID to help with searching.
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_countries()
#' }
mermaid_countries <- function(url = base_url) {
  check_internet()
  choices <- get_endpoint("choices", url = url, limit = NULL)
  dplyr::filter(choices, .data$name == "countries")[["data"]][[1]][["name"]]
}
