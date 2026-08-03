#' Get aggregated metrics for all MERMAID surveys, by site, by date
#'
#' Get aggregated metrics from all surveys associated with each site, by date.
#' Includes data from all methods -- Fish Belt, Benthic LIT, Benthic PIT,
#' Benthic Photo Quadrat, Macroinvertebrate, Bleaching, and Habitat Complexity -- if the data sharing
#' policy for that method is public summary or public (and just includes the sample
#'  unit count otherwise). Does not require authorization.
#'
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_summary_sampleevents()
#' }
mermaid_get_summary_sampleevents <- function(limit = NULL) {
  get_endpoint("summarysampleevents", limit = limit) %>%
  # Unpack all df-cols (protocols, then everything within it)
    unpack_protocols()

}

unpack_protocols <- function(x) {
  protocols_cols <- names(x[["protocols"]])

  x <- x %>%
    tidyr::unpack(tidyselect::all_of("protocols"))

  unpack_all_df_cols(x)
}

unpack_all_df_cols <- function(x) {
  df_cols <- purrr::map(x, inherits, "data.frame") %>%
    unlist()

  df_cols <- names(df_cols[df_cols])

  while (length(df_cols) != 0) {
    x <- unpack_df_cols(x, df_cols)

    df_cols <- purrr::map(x, inherits, "data.frame") %>%
      unlist()

    df_cols <- names(df_cols[df_cols])
  }

  x
}
