#' Get MERMAID endpoint
#'
#' @param endpoint Endpoint
#' @param limit Number of records to get. Use NULL (the default) to get all records.
#' @param url API URL. Defaults to https://api.datamermaid.org
#' @param token API token. Not required for unauthenticated endpoints. Get via \code{\link{mermaid_auth}}
#' @param ... Additional parameters used as needed
mermaid_GET <- function(endpoint, limit = NULL, url = base_url, token = NULL, ...) {
  check_internet()
  limit <- check_limit(limit)

  # Convert endpoint(s) into list
  endpoints <- vector("list", length = length(endpoint))
  names(endpoints) <- endpoint

  # Construct API path
  path <- purrr::map(names(endpoints), construct_api_path, token = token, url = url, limit = limit, ...)
  names(path) <- endpoint

  # Call API and return if "choices" endpoint
  parsed <- purrr::map2(
    path, names(path), ~ get_response(.x, .y, ua = ua, token = token, limit = limit)
  )

  # Convert to tibble and lookup values
  parsed_with_lookup <- purrr::map2(parsed, names(parsed), ~ results_lookup_choices(results = .x, endpoint = .y, url = url, ua = ua, token = token))

  if (length(endpoint) == 1) {
    parsed_with_lookup[[1]]
  } else {
    parsed_with_lookup
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

construct_api_path <- function(endpoint, token, url, limit, ...) {
  # Construct first page - maximum size is 5000
  limit <- ifelse(is.null(limit) || limit > 5000, 5000, limit)

  if (endpoint == "projects" & is.null(token)) {
    # Need showall = TRUE if it's the "projects" endpoint and not an authenticated call
    path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit, showall = TRUE, ...))
  } else {
    path <- httr::modify_url(url, path = paste0("v1/", endpoint), query = list(limit = limit, ...))
  }
}

get_response <- function(path, endpoint, ua, token, limit) {
  if(endpoint == "choices") {
    get_choices_response(path, endpoint, ua, token, limit)
  } else {
    get_paginated_response(path, ua, token, limit)
  }
}

get_choices_response <- function(path, endpoint, ua, token, limit) {
    parsed <- get_and_parse(path = path, ua = ua, token = token)
    res <- tibble::as_tibble(parsed)
    res[["data"]] <- sapply(res[["data"]], tibble::as_tibble)

    if (is.null(limit)) {
      res
    } else {
      head(res, limit)
    }
}

get_paginated_response <- function(path, ua, token, limit) {
  all_res <- list()
  i <- 1
  res <- get_and_parse(path = path, ua = ua, token = token)
  all_res[[i]] <- res[["results"]]
  n_res <- nrow(all_res[[i]])

  while (!is.null(res$`next`) && (is.null(limit) || n_res < limit)) {
    path <- res$`next`
    i <- i + 1
    res <- get_and_parse(path = path, ua = ua, token = token)
    all_res[[i]] <- res[["results"]]
    n_res <- n_res + nrow(all_res[[i]])
  }

  res <- do.call("rbind", all_res)
  res <- tibble::as_tibble(res)

  if (is.null(limit)) {
    res
  } else {
    head(res, limit)
  }
}

get_and_parse <- function(path, ua, token) {
  resp <- httr::GET(path, ua, token)
  check_errors(resp)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
}

results_lookup_choices <- function(results, endpoint, url, ua, token) {
  if (nrow(results) == 0 || ncol(results) == 0) {
    return(
      tibble::tibble()
    )
  }
  if (basename(endpoint) == "sites") {
    choices <- get_and_parse(
      path = httr::modify_url(url, path = "v1/choices"),
      ua = ua, token = token
    )
    choices <- tibble::as_tibble(choices)
    choices[["data"]] <- sapply(choices[["data"]], tibble::as_tibble)

    results <- results %>%
      lookup_variable(choices, "country") %>%
      lookup_variable(choices, "reef_type") %>%
      lookup_variable(choices, "reef_zone") %>%
      lookup_variable(choices, "exposure")

    results <- results %>%
      tidyr::unpack(cols = c(location)) %>%
      tidyr::hoist(coordinates,
        latitude = 2,
        longitude = 1
      ) %>%
      dplyr::select(-type)
  } else if (endpoint == "projects") {
    results <- results %>%
      dplyr::mutate(status = dplyr::recode(status, `10` = "Locked", `80` = "Test", `90` = "Open")) %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::starts_with("data_policy_")),
        ~ dplyr::recode(.x, `10` = "Private", `50` = "Public Summary", `100` = "Public")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        countries = paste0(countries, collapse = "; "),
        tags = paste0(tags, collapse = "; ")
      ) %>%
      dplyr::ungroup()
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
