mermaid_get_summary_sites <- function(limit = NULL) {
  res <- get_endpoint("summarysites", limit = limit)

  # Unpack all df-cols (protocols, then everything within it)
  unpack_protocols(res)
}

summary_sites_columns <- c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "management_regimes", "date_min", "date_max", "data_policy_beltfish", "data_policy_benthiclit", "data_policy_benthicpit", "data_policy_habitatcomplexity", "data_policy_bleachingqc", "project_notes", "site_notes", "contact_link", "protocols")
