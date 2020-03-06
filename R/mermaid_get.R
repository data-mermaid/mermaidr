#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Defaults to 50.
#' @param url API URL. Defaults to https://api.datamermaid.org
#' @param token API token. Not required for unauthenticated endpoints. Get via \code{\link{mermaid_auth}}
#' @param ... Additional parameters used as needed in \code{\link{search_projects}}
mermaid_GET <- function(endpoint, limit = 50, url = base_url, token = NULL, ...) {
  check_internet()

  limit <- check_limit(limit)
  path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit, showall = TRUE, ...))
  resp <- httr::GET(path, ua, token)

  check_errors(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)

  if (endpoint == "choices") {
    parsed
  } else {
    tibble::as_tibble(parsed[["results"]])
  }
}

check_errors <- function(response) {
  if (httr::http_error(response)) {
    stop(paste0(
      "Mermaid API request failed: (", httr::status_code(response), ") ",
      httr::http_status(response)[["reason"]]
    ),
    call. = FALSE
    )
  }
}
