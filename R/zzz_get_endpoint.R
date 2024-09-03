#' Get MERMAID endpoint
#'
#' @inheritParams mermaid_GET
#' @noRd
get_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishfamilies", "fishgenera", "fishspecies", "fishsizes", "managements", "projects", "projecttags", "sites", "summarysampleevents", "summarysites"), limit = NULL, filter = NULL, ...) {
  url <- base_url

  endpoint <- match.arg(endpoint, several.ok = TRUE)

  res <- mermaid_GET(endpoint, limit = limit, filter = filter, ...)

  res_lookups <- purrr::map2(res, names(res), lookup_choices)
  res_strip_name_suffix <- purrr::imap(res_lookups, strip_name_suffix)

  res_columns <- purrr::map2(res_strip_name_suffix, names(res_strip_name_suffix), construct_endpoint_columns)

  if (length(res_columns) > 1) {
    res_columns
  } else {
    res_columns[[endpoint]]
  }
}

lookup_choices <- function(results, endpoint, endpoint_type = "main") {
  url <- base_url

  if (nrow(results) == 0) {
    if (endpoint_type == "main") {
      cols <- mermaid_endpoint_columns[[endpoint]]
    } else if (endpoint_type == "project") {
      cols <- mermaid_project_endpoint_columns[[endpoint]]
      remove_cols <- project_data_df_columns_list[[stringr::str_remove(endpoint, "/csv")]]
      if (!is.null(remove_cols)) {
        cols <- cols[!cols %in% remove_cols]
      }
    }
    if (ncol(results) != 0) {
      cols <- unique(c(names(results), cols))
    }
    results <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(results) <- cols
    return(results)
  }

  if (endpoint == "sites") {
    choices <- mermaid_GET("choices")[["choices"]]

    results <- results %>%
      lookup_variable(choices, "country") %>%
      lookup_variable(choices, "reef_type") %>%
      lookup_variable(choices, "reef_zone") %>%
      lookup_variable(choices, "exposure") %>%
      dplyr::rename_at(c("country_name", "reef_type_name", "reef_zone_name", "exposure_name"), ~ gsub("_name", "", .x))
  } else if (endpoint == "managements") {
    choices <- mermaid_GET("choices")[["choices"]]

    results <- results %>%
      lookup_variable(choices, "parties") %>%
      lookup_variable(choices, "compliance") %>%
      dplyr::rename_at(c("compliance_name", "parties_name"), ~ gsub("_name", "", .x))

    if ("project_name" %in% names(results)) {
      results <- dplyr::rename(results, project = "project_name")
    }
  }

  if ("status" %in% mermaid_endpoint_columns[[endpoint]]) {
    results <- results %>%
      dplyr::mutate(status = dplyr::recode(.data$status, `10` = "Locked", `80` = "Test", `90` = "Open"))
  }

  if (any(grepl("^data_policy_", mermaid_endpoint_columns[["projects"]]))) {
    results <- results %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::starts_with("data_policy_")),
        ~ dplyr::recode(.x, `10` = "Private", `50` = "Public Summary", `100` = "Public")
      )
  }

  results
}

lookup_variable <- function(.data, choices, variable) {
  name <- switch(variable,
    country = "countries",
    reef_type = "reeftypes",
    reef_zone = "reefzones",
    exposure = "reefexposures",
    compliance = "managementcompliances",
    parties = "managementparties"
  )

  variable_names <- choices %>%
    dplyr::filter(name == !!name) %>%
    dplyr::select(-"name") %>%
    tidyr::unnest(data) %>%
    dplyr::select(tidyselect::all_of(c("id", "name"))) %>%
    dplyr::rename_all(~ paste0(variable, "_", .x))

  join_by <- variable
  names(join_by) <- paste0(variable, "_id")

  # Check if there are multiple IDs in .data column, and separate, join, then re-combine
  if (any(stringr::str_detect(.data[[join_by]], ",|;"), na.rm = TRUE)) {
    .data_temp <- .data %>%
      dplyr::mutate(temp_row_for_rejoin = dplyr::row_number())

    .data_sep <- .data_temp %>%
      dplyr::select(tidyselect::all_of(c("temp_row_for_rejoin", join_by))) %>%
      tidyr::separate_rows(tidyselect::all_of(names(join_by)), sep = ", ")

    .data_to_name <- variable_names %>%
      dplyr::right_join(.data_sep, by = names(join_by)) %>%
      dplyr::group_by(.data$temp_row_for_rejoin) %>%
      dplyr::summarise(dplyr::across(tidyselect::all_of(c(names(join_by), paste0(join_by, "_name"))), ~ stringr::str_c(.x, collapse = ", ")))

    .data_temp %>%
      dplyr::left_join(.data_to_name, by = "temp_row_for_rejoin") %>%
      dplyr::select(-tidyselect::all_of(c("temp_row_for_rejoin", join_by)))
  } else {
    # Otherwise, just join by ID
    variable_names %>%
      dplyr::right_join(.data, by = join_by)
  }
}

construct_endpoint_columns <- function(x, endpoint) {
  dplyr::select(x, mermaid_endpoint_columns[[endpoint]])
}

strip_name_suffix <- function(results, endpoint, covariates = FALSE) {
  res_names <- names(results)
  # Remove any _name suffixes, except score_name since we want to keep both score and score_name
  # Convert score_name to score_NAME first (so _name isn't removed from it)
  res_names[which(res_names == "score_name")] <- "score_NAME"

  # Then remove _name from any of the names
  res_names <- gsub("_name", "", res_names)

  # Then convert _NAME back to _name
  res_names <- gsub("_NAME", "_name", res_names)

  names(results) <- res_names

  # Remove IDs, except project ID, sample event/unit ID, site ID (if covariates), or any other IDs included in the requested endpoint
  results[, (!grepl("_id$", names(results))) | (names(results) %in% allowed_ids(endpoint, covariates = covariates))]
}

allowed_ids <- function(endpoint, covariates = FALSE) {
  ids <- c("project_id", "sample_event_id", "sample_unit_id")

  if (endpoint %in% names(mermaid_endpoint_columns)) {
    ids <- c(ids, mermaid_endpoint_columns[[endpoint]][grepl("_id$", mermaid_endpoint_columns[[endpoint]])])
  }

  if (covariates) {
    ids <- c(ids, "site_id")
  }

  unique(ids)
}

# Defined in respective function files
mermaid_endpoint_columns <- list(
  benthicattributes = benthicattributes_columns,
  choices = choices_columns,
  fishfamilies = fishfamilies_columns,
  fishgenera = fishgenera_columns,
  fishspecies = fishspecies_columns,
  fishsizes = fishsizes_columns,
  managements = managements_columns,
  projects = projects_columns,
  projecttags = projecttags_columns,
  sites = sites_columns,
  summarysampleevents = summary_sampleevents_columns
)
