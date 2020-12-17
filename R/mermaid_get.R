#' @param endpoint Endpoint
#' @param limit Number of records to get. Use NULL (the default) to get all records.
#' @param token API token. Not required for unauthenticated endpoints (non project- or user-specific). Get manually via \code{\link{mermaid_auth}} or automatically when running a project- or user-specific function.
#' @param ... Additional parameters used as needed
#'
#' @name mermaid_GET
NULL

#' Get MERMAID endpoint
#'
#' Get MERMAID endpoint. You should not need to call this function directly - please use the \code{mermaid_get_*} functions instead.
#'
#' @inheritParams mermaid_GET
#' @noRd
mermaid_GET <- function(endpoint, limit = NULL, token = NULL, ...) {
  check_internet()
  limit <- check_limit(limit)

  # Convert endpoint(s) into list
  endpoints <- vector("list", length = length(endpoint))
  names(endpoints) <- endpoint

  # Construct API path
  path <- purrr::map(names(endpoints), construct_api_path, token = token, limit = limit, ...)
  names(path) <- endpoint

  # Call API and return results
  res <- purrr::map2(path, basename(names(path)), get_response, ua = ua, token = token, limit = limit)

  # Remove validation column, collapse list-cols
  purrr::map2(res, basename(names(res)), initial_cleanup)
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

construct_api_path <- function(endpoint, token, limit, ...) {
  # Construct first page - maximum size is 5000
  limit <- ifelse(is.null(limit) || limit > 5000, 5000, limit)

  if (endpoint == "projects" & is.null(token)) {
    # Need showall = TRUE if it's the "projects" endpoint and not an authenticated call
    path <- httr::modify_url(base_url, path = paste0("v1/", endpoint, "/"), query = list(limit = limit, showall = TRUE, ...))
  } else {
    path <- httr::modify_url(base_url, path = paste0("v1/", endpoint, "/"), query = list(limit = limit, ...))
  }
}

get_response <- function(path, endpoint, ua, token, limit) {
  if (endpoint == "choices") {
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

  res <- dplyr::bind_rows(all_res)
  res <- tibble::as_tibble(res)

  if (is.null(limit)) {
    res
  } else {
    head(res, limit)
  }
}

get_and_parse <- function(path, ua, token) {
  resp <- suppress_http_warning(httr::RETRY("GET", path, ua, token))
  check_errors(resp)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
}

suppress_http_warning <- function(expr, warning_function = "parse_http_status", warning_regex = "NAs introduced by coercion") {
  withCallingHandlers(expr, warning = function(w) {
    if (length(warning_function) == 1 && length(grep(warning_function, conditionCall(w))) && length(grep(warning_regex, conditionMessage(w)))) {
      invokeRestart("muffleWarning")
    }
  })
}

initial_cleanup <- function(results, endpoint) {
  if (nrow(results) == 0 || ncol(results) == 0) {
    return(
      tibble::tibble()
    )
  }

  if ("validations" %in% names(results)) {
    results <- results %>%
      dplyr::select(-.data$validations)
  }

  if (endpoint == "sites") {
    results <- results %>%
      tidyr::unpack(cols = c(.data$location)) %>%
      tidyr::hoist(.data$coordinates,
        latitude = 2,
        longitude = 1
      ) %>%
      dplyr::select(-.data$type)
  }

  if (endpoint != "choices") {
    results <- collapse_id_name_lists(results)

    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate_if(is_list_col, ~ paste0(.x, collapse = "; ")) %>%
      dplyr::ungroup()
  }

  if (all(c("profile", "profile_name") %in% names(results))) {
    results <- dplyr::select(results, -.data$profile) %>%
      dplyr::rename(profile = .data$profile_name)
  }

  if (all(c("project", "project_name") %in% names(results))) {
    results <- dplyr::select(results, -.data$project) %>%
      dplyr::rename(project = .data$project_name)
  }

  if ("transect_len_surveyed" %in% names(results)) {
    results <- dplyr::rename(results, transect_length = .data$transect_len_surveyed)
  }

  if ("sample_date" %in% names(results)) {
    results <- dplyr::mutate(results, sample_date = as.Date(.data$sample_date))
  }

  results
}

is_list_col <- function(x) {
  is.list(x) && !is.data.frame(x)
}

collapse_id_name_lists <- function(results) {
  list_col_lgl <- purrr::map_lgl(results, inherits, "list")
  list_cols <- names(list_col_lgl[list_col_lgl])

  for (i in seq_along(list_cols)) {
    if (all(c("id", "name") %in% names(results[[list_cols[[i]]]][[1]]))) {
      results <- results %>%
        tidyr::hoist(list_cols[[i]], list_name = "name") %>%
        dplyr::select(-list_cols[[i]]) %>%
        dplyr::rename(!!list_cols[[i]] := .data$list_name)
    }
  }

  results
}
