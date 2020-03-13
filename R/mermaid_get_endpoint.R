#' Get MERMAID Endpoint
#'
#' @inheritParams mermaid_GET
#'
#' @export
mermaid_get_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishattributes", "fishfamilies", "fishgenera", "fishspecies", "managements", "projects", "sites"), limit = 50, url = base_url, ...) {
  endpoint <- match.arg(endpoint)
  res <- mermaid_GET(endpoint, limit = limit, url = url, ...)

  if (nrow(res) == 0) {
    cols <- mermaid_endpoint_columns[[endpoint]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res[, mermaid_endpoint_columns[[endpoint]]]
  }
}
