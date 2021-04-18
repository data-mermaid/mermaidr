#' Expand columns in MERMAID output
#'
#' Expands data-frame columns (df-cols) that come from \code{mermaidr}. Optionally cleans the column names.
#'
#' @param .data Input data
#' @param append_column_prefix Whether to append the df-cols name as a prefix. Defaults to FALSE
#' @param clean_names Whether clean the new column names into \code{snake_case} format, in case they are not clean (e.g. contain spaces, dashes, etc). Note that the name cleaning applies to *all* columns, not just ones resulting from the df-cols.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' sampleunits <- mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_get_project_data("fishbelt", "sampleunits", limit = 1) %>%
#'   select(project, biomass_kgha_by_trophic_group)
#'
#' names(sampleunits)
#' # [1] "project" "biomass_kgha_by_trophic_group"
#'
#' sampleunits <- sampleunits %>%
#'   mermaid_clean_columns()
#'
#' names(sampleunits)
#' # [1] "project"               "piscivore"             "planktivore"
#' # [4] "invertivore_mobile"    "herbivore_detritivore"
#' }
mermaid_clean_columns <- function(.data, append_column_prefix = FALSE, clean_names = TRUE) {
  df_cols <- sapply(.data, function(x) inherits(x, "data.frame"))
  df_cols <- names(df_cols[df_cols])

  df_unpack <- .data %>%
    tidyr::unpack(
      cols = tidyselect::all_of(df_cols),
      names_sep = "_"
    )

  if (!append_column_prefix) {
    df_cols_regex <- paste0(paste0(df_cols, "_"), collapse = "|")

    df_unpack <- df_unpack %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::starts_with(df_cols)),
        function(x) {
          purrr::map_chr(x, rename_other) %>%
            stringr::str_remove_all(df_cols_regex)
        }
      )
  }

  if (clean_names) {
    df_unpack %>%
      janitor::clean_names()
  } else {
    df_unpack
  }
}

rename_other <- function(x) {
  if (stringr::str_ends(x, "other") & stringr::str_detect(x, "by_")) {
    grouping <- stringr::str_split(x, pattern = "by_", simplify = TRUE)[[2]] %>%
      stringr::str_remove_all("_other") %>%
      stringr::str_remove_all("_avg")

    paste0("other_", grouping)
  } else {
    return(x)
  }
}
