#' Check field options for MERMAID import
#'
#' Check that your data matches allowed field options for importing data for a given method into MERMAID.
#'
#' @param data Data to be imported into MERMAID.
#' @param options Field options for the same method as \code{data}, from \code{\link{mermaid_import_get_options}}.
#' @param field Field to check options for (from \code{options}).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' projects <- mermaid_get_my_projects()
#'
#' options <- projects %>%
#'   head(1) %>%
#'   mermaid_import_get_options("fishbelt")
#'
#' data <- tibble(Visibility = c("<1m - bad", "10+m - exellent"))
#'
#' data %>%
#'   mermaid_import_check_options(options, "Visibility")
#'
#' # • Some errors in values of `Visibility` - please check table below
#' # # A tibble: 2 × 3
#' # data_value        closest_choice   match
#' # <chr>             <chr>            <lgl>
#' # 1 10+m - exellent 10+m - excellent FALSE
#' # 2 <1m - bad       <1m - bad        TRUE
mermaid_import_check_options <- function(data, options, field) {
  # Check field exists in options
  options_fields <- names(options)
  if (!field %in% options_fields) {
    stop(
      "`", field, " does not exist in `options`. Possible options are: ", paste0(options_fields, collapse = ", "),
      call. = FALSE
    )
  }

  # Check field exists in data
  data_fields <- names(data)
  if (!field %in% data_fields) {
    stop("`", field, "` column does not exist in `data`.", call. = FALSE)
  }

  # Get field from data
  data_field <- data[field] %>%
    dplyr::distinct()
  names(data_field) <- "data_value"

  # Get field from options
  options_field <- options[[field]]

  # Check if field is required
  options_field_required <- options_field[["required"]]

  if (is.null(options_field_required)) { # Error if required is missing
    stop('`required` field is missing from `options[["', field, '"]]`. Please ensure `options` is output from `mermaid_import_get_options()`.', call. = FALSE)
  }

  # If not required, allow NAs

  # If required, do NOT allow NAs
  # Return message if there are NAs which are not allowed
  if (options_field_required) {
    if (any(is.na(data_field))) {
      usethis::ui_oops("`{field}` is required, but data contains NA values. All values must be non-NA")
      return(invisible())
    }
  } else {
    if (all(is.na(data_field))) {
      usethis::ui_done("All values of `{field}` are NA, no checking to be done")
      return(invisible())
    }
  }

  # Get just possible choices
  options_field <- options_field[["choices"]]

  # If choices is NULL, any value is allowed - return message
  if (is.null(options_field)) {
    usethis::ui_done("Any value is allowed for `{field}` - no checking to be done")
    return(invisible())
  }

  # Check data field against options field (case insensitive)
  names(options_field) <- "choices"

  matches <- data_field %>%
    dplyr::filter(!is.na(.data$data_value)) %>%
    fuzzyjoin::stringdist_left_join(
      options_field,
      by = c("data_value" = "choices"),
      ignore_case = TRUE,
      distance_col = "diff",
      max_dist = Inf
    ) %>%
    dplyr::group_by(.data$data_value) %>%
    dplyr::filter(diff == min(.data$diff)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(match = .data$diff == 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$data_value, closest_choice = .data$choices, .data$match)

  # Return message about whether fields match or do not
  if (all(matches[["match"]])) {
    usethis::ui_done("All values of `{field}` match")
  } else {
    usethis::ui_todo("Some errors in values of `{field}` - please check table below")
  }

  # Return tibble of data vs options and if they are a match
  matches %>%
    dplyr::arrange(.data$match) # Put non-matches first
}