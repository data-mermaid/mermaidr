#' MERMAID countries
#'
#' Returns the names of countries in MERMAID to help with searching
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' mermaid_countries()
mermaid_countries <- function(url = base_url) {
  choices <- mermaid_get_endpoint("choices", url = url, limit = NULL)
  dplyr::filter(choices, name == "countries")[["data"]][[1]][["name"]]
}
