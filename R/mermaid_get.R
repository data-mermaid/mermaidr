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
  resp <- httr::GET(path, ua, httr::add_headers(Authorization = Sys.getenv("MERMAID_API_TOKEN")))

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

#' Get MERMAID endpoints.
#'
#' @description
#'
#' These wrapper functions make it easy to get overall data.
#' There are 12 functions:
#' \itemize{
#'   \item \code{mermaid_benthic_attributes()}
#'   \item \code{mermaid_choices()}
#'   \item \code{mermaid_fish_attributes()}
#'   \item \code{mermaid_fish_families()}
#'   \item \code{mermaid_fish_genera()}
#'   \item \code{mermaid_fish_species()}
#'   \item \code{mermaid_managements()}
#'   \item \code{mermaid_organization_profiles()}
#'   \item \code{mermaid_organizations()}
#'   \item \code{mermaid_profiles()}
#'   \item \code{mermaid_projects()}
#'   \item \code{mermaid_sites()}
#'
#'}
#' @inheritParams mermaid_get
#'
#' @export
#' @examples
#' print("todo")
mermaid_benthic_attributes <- function(limit = 50, results_only = TRUE) {
  mermaid_get("benthicattributes")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_choices <- function(limit = 50, results_only = TRUE) {
  mermaid_get("choices")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_fish_attributes <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishattributes")
}

mermaid_fish_families <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishfamilies")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_fish_genera <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishgenera")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_fish_species <- function(limit = 50, results_only = TRUE) {
  mermaid_get("fishspecies")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_managements <- function(limit = 50, results_only = TRUE) {
  mermaid_get("managements")
}


#' @rdname mermaid_benthic_attributes
#' @export
mermaid_organization_profiles <- function(limit = 50, results_only = TRUE) {
  mermaid_get("organization_profiles")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_organizations <- function(limit = 50, results_only = TRUE) {
  mermaid_get("organizations")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_profiles <- function(limit = 50, results_only = TRUE) {
  mermaid_get("profiles")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_projects <- function(limit = 50, results_only = TRUE) {
  mermaid_get("projects")
}

#' @rdname mermaid_benthic_attributes
#' @export
mermaid_sites <- function(limit = 50, results_only = TRUE) {
  mermaid_get("sites")
}
