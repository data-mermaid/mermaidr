#' Get aggregated metrics for all MERMAID surveys, by site, for all dates
#'
#' Get aggregated metrics from all surveys associated with each site, for all dates. Includes data from all methods - Fish Belt, Benthic LIT, Benthic PIT, Bleaching, and Habitat Complexity - if the data sharing policy for that method is public summary or public (and just includes the sample unit count otherwise). Does not require authorization.
#'
#' @inheritParams mermaid_GET
#'
#' @return
#' @export
#'
#' @examples
#' mermaid_get_summary_sites()
mermaid_get_summary_sites <- function(limit = NULL) {
  res <- get_endpoint("summarysites", limit = limit)

  # Unpack all df-cols (protocols, then everything within it)
  unpack_protocols(res)
}

summary_sites_columns <- c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "management_regimes", "date_min", "date_max", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "project_notes", "site_notes", "contact_link", "protocols")
