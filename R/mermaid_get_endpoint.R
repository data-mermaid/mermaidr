#' Get other MERMAID API endpoints
#'
#' @param endpoint MERMAID API endpoint. One of "choices", "projecttags", "fishsizes".
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \donttest{
#' mermaid_get_endpoint("choices")
#' }
mermaid_get_endpoint <- function(endpoint = c("choices", "projecttags", "fishsizes"), limit = NULL, url = base_url) {
  if (!all(endpoint %in% c("choices", "projecttags", "fishsizes"))) {
    stop('`endpoint` must be one of: "choices", "projecttags", "fishsizes"', call. = FALSE)
  }

  endpoint <- match.arg(endpoint, several.ok = TRUE)

  get_endpoint(endpoint = endpoint, limit = limit, url = url)
}

choices_columns <- c("name", "data")
projecttags_columns <- c("id", "name", "slug", "description", "created_on", "updated_on")
fishsizes_columns <- c("id", "name", "val", "fish_bin_size", "created_on", "updated_on")
