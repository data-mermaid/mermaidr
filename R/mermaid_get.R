#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Defaults to 50.
#' @param url API URL. Defaults to https://api.datamermaid.org
#' @param token API token. Not required for unauthenticated endpoints. Get via \code{\link{mermaid_auth}}
#' @param ... Additional parameters used as needed in \code{\link{mermaid_search_projects}}
mermaid_GET <- function(endpoint, limit = 50, url = base_url, token = NULL, ...) {
  check_internet()
  limit <- check_limit(limit)

  if (endpoint == "projects" & is.null(token)) {
    path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit, showall = TRUE, ...))
  } else {
    path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit, ...))
  }

  parsed <- get_and_parse(path = path, ua = ua, token = token)

  if (endpoint == "choices") {
    res <- tibble::as_tibble(parsed)
    res[["data"]] <- sapply(res[["data"]], tibble::as_tibble)
    res
  } else {
    choices <- get_and_parse(
      path = httr::modify_url(url, path = "v1/choices"),
      ua = ua, token = token
    )
    results_lookup_choices(results = parsed[["results"]], choices = choices, endpoint = endpoint)
  }
}

check_errors <- function(response) {
  if (httr::http_error(response)) {
    stop(paste0(
      "Mermaid API request failed: (", httr::status_code(response), ") ",
      httr::http_status(response)[["reason"]]
    ),
    call. = FALSE
    )
  }
}

get_and_parse <- function(path, ua, token) {
  resp <- httr::GET(path, ua, token)
  check_errors(resp)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
}

results_lookup_choices <- function(results, choices, endpoint) {
  results <- tibble::as_tibble(results)
  choices <- tibble::as_tibble(choices)
  choices[["data"]] <- sapply(choices[["data"]], tibble::as_tibble)

  switch(basename(endpoint),
    sites = {
      results <- results %>%
        lookup_variable(choices, "country") %>%
        lookup_variable(choices, "reef_type") %>%
        lookup_variable(choices, "reef_zone") %>%
        lookup_variable(choices, "exposure")

      results %>%
        tidyr::unpack(cols = c(location)) %>%
        tidyr::hoist(coordinates,
          latitude = 2,
          longitude = 1
        ) %>%
        dplyr::select(-type)
    },
    results
  )
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
