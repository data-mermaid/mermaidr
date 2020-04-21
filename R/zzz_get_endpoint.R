#' Get MERMAID endpoint
#'
#' @inheritParams mermaid_GET
#'
#' @examples
#' get_endpoint("sites", limit = 1)
get_endpoint <- function(endpoint = c("benthicattributes", "choices", "fishfamilies", "fishgenera", "fishspecies", "fishsizes", "managements", "projects", "projecttags", "sites"), limit = NULL, url = base_url, ...) {
  endpoint <- match.arg(endpoint, several.ok = TRUE)
  res <- mermaid_GET(endpoint, limit = limit, url = url, ...)

  res_lookups <- purrr::map2(res, names(res), lookup_choices, url = url)
  res_strip_name_suffix <- purrr::map(res_lookups, strip_name_suffix)

  res_columns <- purrr::map2(res_strip_name_suffix, names(res_strip_name_suffix), construct_endpoint_columns)

  if (length(res_columns) > 1) {
    res_columns
  } else {
    res_columns[[endpoint]]
  }
}

lookup_choices <- function(results, endpoint, url, endpoint_type = "main") {
  if (nrow(results) == 0) {
    if (endpoint_type == "main") {
      cols <- mermaid_endpoint_columns[[endpoint]]
    } else if (endpoint_type == "project") {
      cols <- mermaid_project_endpoint_columns[[endpoint]]
    }
    if (ncol(results) != 0) {
      cols <- unique(c(names(results), cols))
    }
    results <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(results) <- cols
    return(results)
  }

  if (endpoint == "sites") {
    choices <- mermaid_GET("choices", url = url)[["choices"]]

    results <- results %>%
      lookup_variable(choices, "country") %>%
      lookup_variable(choices, "reef_type") %>%
      lookup_variable(choices, "reef_zone") %>%
      lookup_variable(choices, "exposure") %>%
      dplyr::rename_at(dplyr::vars(country_name, reef_type_name, reef_zone_name, exposure_name), ~ gsub("_name", "", .x))
  } else if (endpoint == "managements") {
    if ("project_name" %in% names(results)) {
      results <- dplyr::rename(results, project = project_name)
    }
  }

  if ("status" %in% mermaid_endpoint_columns[[endpoint]]) {
    results <- results %>%
      dplyr::mutate(status = dplyr::recode(status, `10` = "Locked", `80` = "Test", `90` = "Open"))
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
    exposure = "reefexposures"
  )

  variable_names <- choices %>%
    dplyr::filter(name == !!name) %>%
    dplyr::select(-name) %>%
    tidyr::unnest(data) %>%
    dplyr::select(id, name) %>%
    dplyr::rename_all(~ paste0(variable, "_", .x))

  join_by <- variable
  names(join_by) <- paste0(variable, "_id")

  variable_names %>%
    dplyr::right_join(.data, by = join_by)
}

construct_endpoint_columns <- function(x, endpoint) {
  dplyr::select(x, mermaid_endpoint_columns[[endpoint]])
}

strip_name_suffix <- function(results) {
  res_names <- names(results)
  res_names <- gsub("_name", "", res_names)
  names(results) <- res_names

  results[, !grepl("_id$", names(results)) | names(results) == "project_id"]
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
  sites = sites_columns
)
