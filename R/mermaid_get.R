#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Defaults to 50.
#' @param url API URL. Defaults to https://dev-api.datamermaid.org
#' @param token API token. Not required for unauthenticated endpoints. Get via \link{\code{mermaid_token}}
mermaid_GET <- function(endpoint, limit = 50, url = base_url, token = NULL) {
  path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit))
  resp <- httr::GET(path, ua, token)

  check_errors(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE, simplifyDataFrame = TRUE)
  results <- dplyr::as_tibble(parsed[["results"]])

  results
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
