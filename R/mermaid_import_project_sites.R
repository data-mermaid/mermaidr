#' Import project sites into MERMAID Collect
#'
#' @param data Data to import. A data frame containing the fields: \code{name}, \code{latitude}, \code{longitude}, \code{country}, \code{reef_type}, \code{reef_zone}, \code{exposure}, and optionally \code{notes} and \code{project}.
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
#'   mermaid_import_project_sites(data)
#' }
mermaid_import_project_sites <- function(project, data, token = mermaid_token()) {
  # Convert project to ID
  project <- as_id(project)
  check_project(project)

  # Check columns
  col_names <- c("name", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure")
  if (!all(col_names %in% names(data))) {
    stop("`data` must contain columns: ", paste0(col_names, collapse = ", "), call. = FALSE)
  }

  # Check excess columns
  col_names <- c(col_names, "notes", "project")
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

  # Check that latitude and longitude are numeric
  if (!all(is.numeric(data[["latitude"]]) & is.numeric(data[["longitude"]]))) {
    stop("`latitude` and `longitude` must be numeric.", call. = FALSE)
  }

  # Check country, exposure, reef type, reef zone
  # If not matching options, then error and show valid options
  # If all good, get IDs

  # Get choices
  choices <- mermaid_get_endpoint("choices")

  # Iterate over country, reef_type, reef_zone, exposure
  # Check if values match, and if so, convert to IDs
  for (col in c("country", "reef_type", "reef_zone", "exposure")) {
    choices_col <- switch(col,
      "country" = "countries",
      "reef_type" = "reeftypes",
      "reef_zone" = "reefzones",
      "exposure" = "reefexposures"
    )

    col_choices <- choices %>%
      get_choice_from_choices(choices_col) %>%
      dplyr::rename({{ col }} := .data$name)

    data <- data %>%
      dplyr::left_join(col_choices, by = col)

    # If any `id` are NA, error
    invalid_values <- data %>%
      dplyr::filter(is.na(.data$id)) %>%
      dplyr::pull({{ col }})

    if (length(invalid_values) > 0) {
      # TODO: if country, tell them to use mermaid_countries() instead to see valid choices

      message("Not all values of `", col, "` are valid. Invalid values: ", paste0(invalid_values, collapse = ", "), ". Valid values below:")
      return(col_choices %>%
        dplyr::select({{ col }}))
    }

    # Otherwise, replace col with id
    data <- data %>%
      dplyr::select(-{{ col }}) %>%
      dplyr::rename({{ col }} := .data$id)
  }

  # Post row by row
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
        # Convert lat/long to a single location col, with format list(type = "Point", coordinates = c(lat, long))

        data[["location"]] <- list(type = "Point", coordinates = c(data[["longitude"]], data[["latitude"]]))
        data[["latitude"]] <- data[["longitude"]] <- NULL

        # Convert any NA notes to ""
        if ("notes" %in% names(data)) {
          if (is.na(data[["notes"]])) {
            data[["notes"]] <- ""
          }
        }

        # Set up path to post to
        path <- glue::glue("{base_url}/v1/projects/{project}/sites/")

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

    usethis::ui_todo("Not all sites imported successfully. The following rows did not import: {paste(failed_post, collapse = ',')}")
  } else {
    usethis::ui_done("All sites imported!")
  }
}

post_data <- function(path, data, token = mermaid_token()) {
  resp <- suppress_http_warning(httr::RETRY("POST", path, encode = "json", body = data, ua, token, terminate_on = c(401, 403, 400)))

  resp$status_code
}

get_choice_from_choices <- function(choices, name) {
  dplyr::filter(choices, .data$name == !!name)[["data"]][[1]] %>%
    dplyr::select(.data$id, .data$name)
}
