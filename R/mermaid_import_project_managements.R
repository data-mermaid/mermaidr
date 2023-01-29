#' Import project management regimes into MERMAID Collect
#'
#' @param data Data to import. A data frame containing \code{name}, and optionally \code{name_secondary}, \code{est_year} (numeric), \code{size} (numeric, in hectares), \code{parties} (separated by ;), \code{compliance}, \code{notes}, and \code{project}. The data \strong{must} contain at least one rule with \code{TRUE}. The rules columns are:  \code{open_access}, \code{no_take}, \code{access_restriction}, \code{periodic_closure}, \code{size_limits}, \code{gear_restriction}. Any missing rules will be treated as \code{FALSE}. All rules columns can be included (with \code{FALSE}) if they are not applicable for the given management regime (i.e., in the case of importing many MRs at once with different rules).
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' project <- mermaid_search_my_projects("Sharla test", include_test_projects = TRUE)
#'
#' data <- dplyr::tibble(
#'   name = "Test management regime",
#'   est_year = 2020,
#'   size = 5,
#'   parties = "NGO; private sector",
#'   compliance = "full",
#'   access_restriction = TRUE,
#'   periodic_closure = TRUE,
#' )
#'
#' project %>%
#'   mermaid_import_project_managements(data)
#' }
mermaid_import_project_managements <- function(project, data, token = mermaid_token()) {
  # Convert project to ID
  project <- as_id(project)
  check_project(project)

  # Check columns - required ----
  required_columns <- c("name")
  if (!all(required_columns %in% names(data))) {
    stop("`data` must contain columns: ", paste0(required_columns, collapse = ", "), call. = FALSE)
  }

  # Check excess columns (compared with optional) -----
  optional_columns <- c("name_secondary", "est_year", "size", "parties", "compliance", "open_access", "no_take", "access_restriction", "periodic_closure", "size_limits", "gear_restriction", "species_restriction", "notes", "project")
  col_names <- c(required_columns, optional_columns)
  if (!all(names(data) %in% col_names)) {
    stop("`data` can only contain columns: ", paste0(col_names, collapse = ", "), call. = FALSE)
  }

  # If there is already project column, check if matches `project`
  if ("project" %in% names(data)) {
    if (!all(data[["project"]] == project)) {
      stop("`project` column does not match project being imported into.", call. = FALSE)
    }
  } else {
    # Append project id to df
    data <- data %>%
      dplyr::mutate(project = project)
  }

  # Check that est_year and size are numeric
  if (!(all(is.na(data[["size"]])))) {
    if (!all(is.numeric(data[["size"]]), na.rm = TRUE)) {
      stop("`size` must be numeric.", call. = FALSE)
    }
  }
  if (!(all(is.na(data[["est_year"]])))) {
    if (!all(is.numeric(data[["est_year"]]), na.rm = TRUE)) {
      stop("`est_year` must be numeric.", call. = FALSE)
    }
  }

  # Check compliance, parties ----
  # If not matching options, then error and show valid options
  # If all good, get IDs

  choices_cols <- c("compliance", "parties")

  # Get choices
  if (any(choices_cols %in% names(data))) {
    choices <- mermaid_get_endpoint("choices")
  }

  # Iterate over compliance, parties
  # Check if values match, and if so, convert to IDs
  # For parties, since multiple are allowed, collapse into a vector
  for (col in choices_cols) {
    if (col %in% names(data)) {
      choices_col <- switch(col,
        compliance = "managementcompliances",
        parties = "managementparties"
      )

      col_choices <- choices %>%
        get_choice_from_choices(choices_col) %>%
        dplyr::mutate(name = tolower(.data$name)) %>%
        dplyr::rename({{ col }} := .data$name)

      if (col == "parties") {
        .data_temp <- data %>%
          dplyr::mutate(temp_row_for_rejoin = dplyr::row_number())

        .data_sep <- .data_temp %>%
          dplyr::select(.data$temp_row_for_rejoin, .data$parties) %>%
          tidyr::separate_rows(.data$parties, sep = ";") %>%
          dplyr::mutate(
            parties = stringr::str_trim(.data$parties),
            parties = tolower(.data$parties)
          )

        .data_to_id <- col_choices %>%
          dplyr::right_join(.data_sep, by = "parties")

        # Check that are are valid
        invalid_values <- .data_to_id %>%
          dplyr::filter(is.na(.data$id) & !is.na(.data$parties)) %>%
          dplyr::pull({{ col }})

        if (length(invalid_values) > 0) {
          message("Not all values of `", col, "` are valid. Invalid values: '", paste0(invalid_values, collapse = "', '"), "'. Values should be separated by ;. Valid values below:")
          return(col_choices %>%
            dplyr::select({{ col }}))
        }

        .data_to_id <- .data_to_id %>%
          dplyr::select(.data$temp_row_for_rejoin, .data$id) %>%
          dplyr::group_nest(.data$temp_row_for_rejoin, .key = "parties")

        data <- .data_temp %>%
          dplyr::select(-.data$parties) %>%
          dplyr::left_join(.data_to_id, by = "temp_row_for_rejoin") %>%
          dplyr::select(-.data$temp_row_for_rejoin)
      } else if (col == "compliance") {
        data <- data %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(col), tolower)) %>%
          dplyr::left_join(col_choices, by = col)

        invalid_values <- data %>%
          dplyr::filter(is.na(.data$id) & !is.na(.data$compliance)) %>%
          dplyr::pull({{ col }})

        if (length(invalid_values) > 0) {
          message("Not all values of `", col, "` are valid. Invalid values: ", paste0(invalid_values, collapse = ", "), ". Valid values below:")
          return(col_choices %>%
            dplyr::select({{ col }}))
        }

        # Otherwise, replace col with id
        data <- data %>%
          dplyr::select(-{{ col }}) %>%
          dplyr::rename({{ col }} := .data$id)
      }
    }
  }

  # Check rules ----
  partial_restrictions_cols <- c("access_restriction", "periodic_closure", "size_limits", "gear_restriction", "species_restriction")
  rules_cols <- c("open_access", "no_take", partial_restrictions_cols)

  ## At least one rule ----

  # Missing cols
  missing_rules_cols <- !any(rules_cols %in% names(data))

  # All (present) FALSE
  if (!missing_rules_cols) {
    all_false <- data %>%
      dplyr::mutate(.temp_row = dplyr::row_number()) %>%
      dplyr::select(.data$.temp_row, tidyselect::any_of(rules_cols)) %>%
      tidyr::pivot_longer(tidyselect::any_of(rules_cols)) %>%
      dplyr::group_by(.data$.temp_row) %>%
      dplyr::summarise(all_false = all(!.data$value)) %>%
      dplyr::filter(.data$all_false) %>%
      nrow() > 0
  } else {
    all_false <- FALSE
  }

  if (missing_rules_cols | all_false) {
    stop("data must contain at least one of the rules: ", paste0(rules_cols, collapse = ", "), " with value TRUE.", call. = FALSE)
  }

  ## No conflict of rules

  ### No open/no take ----
  if (all(c("no_take", "open_access") %in% names(data))) {
    open_no_take <- data %>%
      dplyr::filter(.data$open_access & .data$no_take) %>%
      nrow() > 0

    if (open_no_take) {
      stop("Cannot have both `open_access` and `no_take` as TRUE.", call. = FALSE)
    }
  }

  ### No open/partial ----
  if ("open_access" %in% names(data) & any(partial_restrictions_cols %in% names(data))) {
    open_and_partial <- data %>%
      dplyr::mutate(.temp_row = dplyr::row_number()) %>%
      dplyr::select(.data$.temp_row, .data$open_access, tidyselect::any_of(partial_restrictions_cols)) %>%
      tidyr::pivot_longer(tidyselect::any_of(partial_restrictions_cols)) %>%
      dplyr::group_by(.data$.temp_row, .data$open_access) %>%
      dplyr::summarise(any_partial_rules = any(.data$value)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$open_access & .data$any_partial_rules) %>%
      nrow() > 0

    if (open_and_partial) {
      stop("Cannot have both `open_access` and any partial restrictions rules (`", paste0(partial_restrictions_cols, collapse = "`, `"), "`) as TRUE.", call. = FALSE)
    }
  }

  ### No "no take"/partial -----
  if ("no_take" %in% names(data) & any(partial_restrictions_cols %in% names(data))) {
    open_and_partial <- data %>%
      dplyr::mutate(.temp_row = dplyr::row_number()) %>%
      dplyr::select(.data$.temp_row, .data$no_take, tidyselect::any_of(partial_restrictions_cols)) %>%
      tidyr::pivot_longer(tidyselect::any_of(partial_restrictions_cols)) %>%
      dplyr::group_by(.data$.temp_row, .data$no_take) %>%
      dplyr::summarise(any_partial_rules = any(.data$value)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$no_take & .data$any_partial_rules) %>%
      nrow() > 0

    if (open_and_partial) {
      stop("Cannot have both `no_take` and any partial restrictions rules (`", paste0(partial_restrictions_cols, collapse = "`, `"), "`) as TRUE.", call. = FALSE)
    }
  }

  # Post row by row ----
  data <- data %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    split(.$row)

  site_posts <- data %>%
    purrr::map_dfr(
      function(data) {
        post_row <- data[["row"]]
        # Convert each row to a list
        data <- data %>%
          dplyr::select(-.data$row) %>%
          as.list()

        # Convert parties to a list if there is 1, keep as vector if > 1
        # Weird unboxing bug when converting to JSON
        data[["parties"]] <- data$parties[[1]][["id"]]

        if (length(data[["parties"]] == 1)) {
          data[["parties"]] <- as.list(data[["parties"]])
        }

        # Convert any NA notes to ""
        if ("notes" %in% names(data)) {
          if (is.na(data[["notes"]])) {
            data[["notes"]] <- ""
          }
        }

        # Any other NA columns, set to NULL
        for (col in names(data)) {
          if (all(is.na(data[[col]]))) {
            data[col] <- list(NULL)
          }
        }

        # Set up path to post to
        path <- glue::glue("{base_url}/v1/projects/{project}/managements/")

        # Returns the status code
        status_code <- post_data(path, data)

        dplyr::tibble(
          row = post_row,
          status_code = status_code
        )
      }
    )

  if (!all(site_posts[["status_code"]] == 201)) {
    failed_post <- site_posts %>%
      dplyr::filter(.data$status_code != 201) %>%
      dplyr::pull(.data$row)

    usethis::ui_todo("Not all managements imported successfully. The following rows did not import: {paste(failed_post, collapse = ', ')}")
  } else {
    usethis::ui_done("All managements imported!")
  }
}
