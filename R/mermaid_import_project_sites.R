mermaid_import_project_sites <- function(project, data, token = mermaid_token()) {
  # Convert project to ID
  project <- as_id(project)
  check_project(project)

  # Check columns
  col_names <- c("name", "latitude", "longitude", "notes", "country", "reef_type", "reef_zone", "exposure")
  if (!all(col_names %in% names(data))){
    stop("`data` must contain columns: ", paste0(col_names, collapse = ", "), call. = FALSE)
  }

  # Append project id to df
  data <- data %>%
    dplyr::mutate(project = project)

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
      dplyr::rename({{ col }} := name)

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
      dplyr::rename({{ col }} := id)
  }

  # Post row by row
  # Convert each row to a list
  data <- data %>% as.list()
  # Convert lat/long to a single location col, with format list(type = "Point", coordinates = c(lat, long))
  #   "location": {
  #     "type": "Point",
  #     "coordinates": [
  #       178.4793,
  #       -17.2792
  #     ]
  data[["location"]] <- list(type = "Point", coordinates = c(data[["longitude"]], data[["latitude"]]))
  data[["latitude"]] <- data[["longitude"]] <- NULL

  # Convert any NA notes to ""
  if (is.na(data[["notes"]])) {
    data[["notes"]] <- ""
  }

  # Set up path to post to
  browser()
  path <- glue::glue("{base_url}/v1/projects/{project}/sites/")

  # This returns the status code, so check that they are all 201
  post_data(path, data)

}

post_data <- function(path, data, token = mermaid_token()) {
  resp <- suppress_http_warning(httr::RETRY("POST", path, encode = "json", body = data, ua, token, terminate_on = c(401, 403)))
  check_errors(resp)

  resp$status_code
}

get_choice_from_choices <- function(choices, name) {
  dplyr::filter(choices, .data$name == !!name)[["data"]][[1]] %>%
    dplyr::select(.data$id, .data$name)
}
