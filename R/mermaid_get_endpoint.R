#' Get MERMAID Endpoint
#'
#' @inheritParams mermaid_GET
#'
#' @export
mermaid_get_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishattributes", "fishfamilies", "fishgenera", "fishspecies", "managements", "projects", "sites"), limit = NULL, url = base_url, ...) {
  endpoint <- match.arg(endpoint, several.ok = TRUE)
  res <- mermaid_GET(endpoint, limit = limit, url = url, ...)

  if (length(endpoint) == 1) {
    construct_endpoint_columns(res, endpoint)
  } else {
    purrr::map2(res, names(res), construct_endpoint_columns)
  }
}

construct_endpoint_columns <- function(x, endpoint) {
  if (nrow(x) == 0) {
    cols <- mermaid_endpoint_columns[[endpoint]]
    x <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(x) <- cols
    x
  } else {
    x[, mermaid_endpoint_columns[[endpoint]]]
  }
}
