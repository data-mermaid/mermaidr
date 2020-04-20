#' Get other MERMAID API endpoints
#'
#' @param endpoint MERMAID API endpoint. One of "choices", "projecttags", "fishsizes".
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' mermaid_get_endpoint("choices")
mermaid_get_endpoint <- function(endpoint = c("choices", "projecttags", "fishsizes"), limit = NULL, url = base_url) {

  endpoint <- match.arg(endpoint, several.ok = TRUE)

  get_endpoint(endpoint = endpoint, limit = limit, url = url)

}
