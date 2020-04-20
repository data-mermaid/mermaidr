#' Get MERMAID managements
#'
#' Get a data frame of all MERMAID managements (not project specific).
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' mermaid_get_managements(limit = 10)
mermaid_get_managements <- function(limit = NULL, url = base_url) {
  get_endpoint("managements", limit = limit, url = base_url)
}
