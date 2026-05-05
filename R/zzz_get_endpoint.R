#' Get MERMAID endpoint
#'
#' @inheritParams mermaid_GET
#' @noRd
get_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishfamilies", "fishgenera", "fishspecies", "fishsizes", "managements", "projects", "projecttags", "sites", "summarysampleevents", "classification/labelmappings", "invertattributes", "invertspecies"), limit = NULL, filter = NULL, ...) {
  url <- base_url

  endpoint <- match.arg(endpoint, several.ok = TRUE)

  res <- mermaid_GET(endpoint, limit = limit, filter = filter, ...)

  res_lookups <- purrr::map2(res, names(res), lookup_choices)
  res <- purrr::imap(res_lookups, strip_name_suffix)
  endpoint <- names(res)

  if (!endpoint %in% tested_endpoints) { # WIP, for development
    browser()
    res <- purrr::map2(
      res,
      names(res),
      construct_endpoint_columns
    )
  } else {
    # res <- purrr::map2(
    #   res,
    #   names(res),
    #   remove_blacklist_endpoint_columns
    # )
  }

  # Replace any "" or "NA" with NAs
  # TODO -> this could happen more universally, in mermaid_GET
  res <- purrr::map(res, \(x) x %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        \(y) ifelse(y %in% c("NA", ""),
          NA_character_, y
        )
      )
    ))

  if (length(res) > 1) {
    res
  } else {
    res[[endpoint]]
  }
}

lookup_choices <- function(results, endpoint, endpoint_type = "main") {
  url <- base_url

  if (nrow(results) == 0) {
    browser()
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
    results <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)),
      .name_repair = "minimal"
    )
    names(results) <- cols
    return(results)
  }

  # TODO -> these are pretty specific, should they move?
  if (endpoint == "sites") {
    choices <- mermaid_GET("choices")[["choices"]]

    results <- results %>%
      lookup_variable(choices, "country") %>%
      lookup_variable(choices, "reef_type") %>%
      lookup_variable(choices, "reef_zone") %>%
      lookup_variable(choices, "exposure") %>%
      dplyr::rename_with(~ stringr::str_remove(.x, "_name"))
  } else if (endpoint == "managements") {
    choices <- mermaid_GET("choices")[["choices"]]
    col_order <- names(results)

    results <- results %>%
      lookup_variable(choices, "parties") %>%
      lookup_variable(choices, "compliance") %>%
      dplyr::rename_at(c("compliance_name", "parties_name"), ~ gsub("_name", "", .x))

    if ("project_name" %in% names(results)) {
      results <- dplyr::rename(results, project = "project_name")
    }

    # Keep original order of columns
    results <- results %>%
      dplyr::select(dplyr::all_of(col_order))


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
      dplyr::summarise(
        dplyr::across(
          tidyselect::all_of(c(
            names(join_by),
            paste0(join_by, "_name")
          )),
          ~ stringr::str_c(.x, collapse = ", ")
        )
      )

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

remove_blacklist_endpoint_columns <- function(res, endpoint) {
  # browser()
  if (!endpoint %in% names(blacklist_columns)) {
    browser()
  }
  res %>%
    dplyr::select(-dplyr::any_of(blacklist_columns[[endpoint]]))
}

strip_name_suffix <- function(results, endpoint, covariates = FALSE) {
  if (!endpoint %in% tested_endpoints) {
    browser()
  }
  res_names <- names(results)
  # Remove any _name suffixes, except score_name since we want to keep both score and score_name
  # Convert score_name to score_NAME first (so _name isn't removed from it)
  res_names[which(res_names == "score_name")] <- "score_NAME"

  # do the same with display_name
  res_names[which(res_names == "display_name")] <- "display_NAME"

  # Then remove _name from any of the names
  res_names <- gsub("_name", "", res_names)

  # Then convert _NAME back to _name
  res_names <- gsub("_NAME", "_name", res_names)

  names(results) <- res_names

  # Remove IDs, except project ID, sample event/unit ID, site ID (if covariates),
  # or any other IDs included in the requested endpoint
  results[, (!grepl(
    "_id$",
    names(results)
  )) |
    (names(results) %in% allowed_ids(endpoint, covariates = covariates))]
}

allowed_ids <- function(endpoint, covariates = FALSE) {
  ids <- c("project_id", "sample_event_id", "sample_unit_id")

  if (endpoint %in% names(mermaid_endpoint_columns)) {
    # TODO -> this is explicit white listing, so will need to move away from this
    if (any(stringr::str_ends(mermaid_endpoint_columns[[endpoint]], "_id"))) {
      browser()
      ids <- c(
        ids,
        mermaid_endpoint_columns[[endpoint]][grepl(
          "_id$",
          mermaid_endpoint_columns[[endpoint]]
        )]
      )
    }
  }

  if (covariates) {
    ids <- c(ids, "site_id")
  }

  unique(ids)
}

# Defined in respective function files
mermaid_endpoint_columns <- list(
  choices = choices_columns,
  fishsizes = fishsizes_columns,
  projects = projects_columns,
  projecttags = projecttags_columns,
  summarysampleevents = summary_sampleevents_columns,
  "classification/labelmappings" = classification_labelmappings_columns,
  invertattributes = invertattributes_columns,
  invertspecies = invertspecies_columns
)
