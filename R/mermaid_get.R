#' Get MERMAID endpoint
#'
#' @param limit Number of records to get. Defaults to 50.
#' @param results_only Whether to return the results only (the default) or to return the entire API call (contents, results, path, and response).
#'
#' @export
#' @examples
#' \dontrun{
#' mermaid_endpoint()
#' }
mermaid_get <- function(endpoint, limit = 50, results_only = TRUE) {
  path <- httr::modify_url(base_url, path = paste0("v1/", endpoint), query = list(limit = limit))
  resp <- httr::GET(path, ua, httr::add_headers(Authorization = paste("Bearer", Sys.getenv("MERMAID_API_TOKEN"))))

  check_json(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE, simplifyDataFrame = TRUE)
  results <- dplyr::as_tibble(parsed[["results"]])

  check_errors(resp)

  if (results_only) {
    results
  }
  else {
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
}

#' Get MERMAID benthic attributes endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_benthic_attributes <- function(limit = 50, results_only = TRUE) {
  mermaid_get("benthicattributes")
}

#' Get MERMAID choices endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_choices <- function(limit = 50, results_only = TRUE) {
  mermaid_get("choices")
}

#' Get MERMAID fish attributes endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_attributes <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishattributes")
}

#' Get MERMAID fish families endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_families <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishfamilies")
}

#' Get MERMAID fish genera endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_genera <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishgenera")
}

#' Get MERMAID fish species endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_species <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishspecies")
}

#' Get MERMAID management endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_managements <- function(limit = 50, results_only = TRUE) {
  mermaid_get("managements")
}

#' Get MERMAID organization profiles endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_organization_profiles <- function(limit = 50, results_only = TRUE) {
  mermaid_get("organization_profiles")
}

#' Get MERMAID organizations endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_organizations <- function(limit = 50, results_only = TRUE) {
  mermaid_get("organizations")
}

#' Get MERMAID profiles endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_profiles <- function(limit = 50, results_only = TRUE) {
  mermaid_get("profiles")
}

#' Get MERMAID projects endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_projects <- function(limit = 50, results_only = TRUE) {
  mermaid_get("projects")
}

#' Get MERMAID sites endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_sites <- function(limit = 50, results_only = TRUE) {
  mermaid_get("sites")
}
