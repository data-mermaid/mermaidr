#' Get MERMAID Projects
#'
#' Get MERMAID projects. Returns the projects
#'
#' @param limit Number of projects to get. Defaults to 50.
#'
#' @export
#' @examples \dontrun{
#' mermaid_projects()
#' }
mermaid_projects <- function(limit = 50) {
  path <- httr::modify_url(base_url, path = "v1/projects", query = list(limit = limit))
  resp <- httr::GET(path, ua)

  check_json(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE, simplifyDataFrame = TRUE)
  results <- dplyr::as_tibble(parsed[["results"]])

  check_errors(resp)

  structure(
    list(
      content = parsed,
      results = results,
      path = path,
      response = resp
    ),
    class = "mermaid_api"
  )
}
