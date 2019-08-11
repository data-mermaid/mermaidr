#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Defaults to 50.
#'
#' @export
mermaid_get <- function(endpoint, limit = 50) {
  path <- httr::modify_url(base_url, path = paste0("v1/", endpoint), query = list(limit = limit))
  resp <- httr::GET(path, ua, httr::add_headers(Authorization = paste("Bearer", Sys.getenv("MERMAID_API_TOKEN"))))

  check_json(resp)

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE, simplifyDataFrame = TRUE)
  results <- dplyr::as_tibble(parsed[["results"]])

  check_errors(resp, parsed)

  results
}

#' Get MERMAID benthic attributes endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_benthic_attributes <- function(limit = 50) {
  mermaid_get("benthicattributes", limit = limit)
}

#' Get MERMAID choices endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_choices <- function(limit = 50) {
  mermaid_get("choices", limit = limit)
}

#' Get MERMAID fish attributes endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_attributes <- function(limit = 50) {
  mermaid_get("fishattributes", limit = limit)
}

#' Get MERMAID fish families endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_families <- function(limit = 50) {
  mermaid_get("fishfamilies", limit = limit)
}

#' Get MERMAID fish genera endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_genera <- function(limit = 50) {
  mermaid_get("fishgenera", limit = limit)
}

#' Get MERMAID fish species endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_fish_species <- function(limit = 50) {
  mermaid_get("fishspecies", limit = limit)
}

#' Get MERMAID management endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_managements <- function(limit = 50) {
  mermaid_get("managements", limit = limit)
}

#' Get MERMAID organization profiles endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_organization_profiles <- function(limit = 50) {
  mermaid_get("organization_profiles", limit = limit)
}

#' Get MERMAID organizations endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_organizations <- function(limit = 50) {
  mermaid_get("organizations", limit = limit)
}

#' Get MERMAID profiles endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_profiles <- function(limit = 50) {
  mermaid_get("profiles", limit = limit)
}

#' Get MERMAID projects endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_projects <- function(limit = 50) {
  mermaid_get("projects", limit = limit)
}

#' Get MERMAID sites endpoint.
#'
#' @inheritParams mermaid_get
#' @family MERMAID endpoints
#' @export
get_mermaid_sites <- function(limit = 50) {
  mermaid_get("sites", limit = limit)
}
