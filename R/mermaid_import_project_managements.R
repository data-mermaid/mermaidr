#' Import project managements into MERMAID Collect
#'
#' @param data Data to import. A data frame containing the fields: \code{name}, \code{latitude}, \code{longitude}, \code{country}, \code{reef_type}, \code{reef_zone}, \code{exposure}, and optionally \code{notes}.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' project <- mermaid_search_my_projects("Sharla test", include_test_projects = TRUE)
#'
#' data <- dplyr::tibble(
#'   name = "Test site",
#'   latitude = 43.651070,
#'   longitude = -79.347015,
#'   notes = NA,
#'   country = "Canada",
#'   reef_type = "fringing",
#'   reef_zone = "crest",
#'   exposure = "semi-exposed"
#' )
#'
#' project %>%
#'   mermaid_import_project_managements(data)
#' }
mermaid_import_project_managements <- function(project, data, token = mermaid_token()) {
  # Convert project to ID
  project <- as_id(project)
  check_project(project)

  # Check columns - required - TODO: at least one rule is required
  required_columns <- c("name")
  if (!all(required_columns %in% names(data))) {
    stop("`data` must contain columns: ", paste0(required_columns, collapse = ", "), call. = FALSE)
  }

  # Check excess columns (compared with optional)
  optional_columns <- c("name_secondary", "est_year", "size", "parties", "compliance", "open_access", "no_take", "access_restriction", "periodic_closure", "size_limits", "gear_restriction", "species_restriction", "notes")
  col_names <- c(required_columns, optional_columns)
  if (!all(names(data) %in% col_names)) {
    stop("`data` can only contain columns: ", paste0(col_names, collapse = ", "), call. = FALSE)
  }

  # Append project id to df
  data <- data %>%
    dplyr::mutate(project = project)

  # Check that est_year and size are numeric
  if (!all(is.numeric(data[["size"]]), na.rm = TRUE)) {
    stop("`est_year` and `size` must be numeric.", call. = FALSE)
  }

  # Check country, exposure, reef type, reef zone
  # If not matching options, then error and show valid options
  # If all good, get IDs

  # Get choices
  choices <- mermaid_get_endpoint("choices")

  # Iterate over compliance, parties
  # Check if values match, and if so, convert to IDs
  # For parties, since multiple are allowed, collapse into a vector
  for (col in c("compliance", "parties")) {
    choices_col <- switch(col,
      compliance = "managementcompliances",
      parties = "managementparties"
    )

    col_choices <- choices %>%
      get_choice_from_choices(choices_col) %>%
      dplyr::rename({{ col }} := name)

    if (col == "parties") {
      .data_temp <- data %>%
        dplyr::mutate(temp_row_for_rejoin = dplyr::row_number())

      .data_sep <- .data_temp %>%
        dplyr::select(.data$temp_row_for_rejoin, parties) %>%
        tidyr::separate_rows(.data$parties, sep = "; ")

      # TODO - check that all are valid

      .data_to_id <- col_choices %>%
        dplyr::right_join(.data_sep, by = "parties") %>%
        dplyr::select(.data$temp_row_for_rejoin, .data$id) %>%
        dplyr::group_nest(.data$temp_row_for_rejoin, .key = "parties")

      data <- .data_temp %>%
        dplyr::select(-.data$parties) %>%
        dplyr::left_join(.data_to_id, by = "temp_row_for_rejoin") %>%
        dplyr::select(-.data$temp_row_for_rejoin)
    } else {
      data <- data %>%
        dplyr::left_join(col_choices, by = col)

      # If any `id` are NA, error
      # TODO - what if value is just NA?
      invalid_values <- data %>%
        dplyr::filter(is.na(.data$id)) %>%
        dplyr::pull({{ col }})

      if (length(invalid_values) > 0) {
        message("Not all values of `", col, "` are valid. Invalid values: ", paste0(invalid_values, collapse = ", "), ". Valid values below:")
        return(col_choices %>%
          dplyr::select({{ col }}))
      }

      # Otherwise, replace col with id
      data <- data %>%
        dplyr::select(-{{ col }}) %>%
        dplyr::rename({{ col }} := id)
    }
  }

  # Post row by row
  data <- data %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    split(.$row)

  site_posts <- data %>%
    purrr::map_dfr(
      function(data) {
        post_row <- data[["row"]]
        browser()
        # Convert each row to a list
        data <- data %>%
          dplyr::select(-.data$row) %>%
          as.list()

        # Convert parties to a single vector
        data[["parties"]] <- data$parties[[1]][["id"]]

        # Convert any NA notes to ""
        if ("notes" %in% names(data)) {
          if (is.na(data[["notes"]])) {
            data[["notes"]] <- ""
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

  browser()

  if (!all(site_posts[["status_code"]] == 201)) {
    failed_post <- site_posts %>%
      dplyr::filter(status_code != 201) %>%
      dplyr::pull(row)

    usethis::ui_todo("Not all managements imported successfully. The following rows did not import: {paste(failed_post, collapse = ',')}")
  } else {
    usethis::ui_done("All managements imported!")
  }
}

post_data <- function(path, data, token = mermaid_token()) {
  resp <- suppress_http_warning(httr::RETRY("POST", path, encode = "json", body = data, ua, token, terminate_on = c(401, 403)))

  resp$status_code
}

get_choice_from_choices <- function(choices, name) {
  dplyr::filter(choices, .data$name == !!name)[["data"]][[1]] %>%
    dplyr::select(.data$id, .data$name)
}
