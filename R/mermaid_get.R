#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Defaults to 50.
#' @param url API URL. Defaults to https://dev-api.datamermaid.org
mermaid_GET <- function(endpoint, limit = 50, url = base_url, token = NULL) {
  path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit))
  resp <- httr::GET(path, ua, token)

  check_json(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE, simplifyDataFrame = TRUE)
  results <- dplyr::as_tibble(parsed[["results"]])

  check_errors(resp, parsed)

  results
}
