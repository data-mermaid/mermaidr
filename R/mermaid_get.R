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

initial_cleanup <- function(results, endpoint) {
  if (nrow(results) == 0 || ncol(results) == 0) {
    return(
      tibble::tibble()
    )
  }

  if ("validations" %in% names(results)) {
    results <- results %>%
      dplyr::select(-validations)
  }

  if (endpoint == "sites") {
    results <- results %>%
      tidyr::unpack(cols = c(location)) %>%
      tidyr::hoist(coordinates,
                   latitude = 2,
                   longitude = 1
      ) %>%
      dplyr::select(-type)
  }

  if (endpoint != "choices") {
    results <- collapse_id_name_lists(results)

    results <- results %>%
      dplyr::rowwise() %>%
      dplyr::mutate_if(is_list_col, ~ paste0(.x, collapse = "; ")) %>%
      dplyr::ungroup()
  }

  if (all(c("profile", "profile_name") %in% names(results))) {
    results <- dplyr::select(results, -profile) %>%
      dplyr::rename(profile = profile_name)
  }

  if (all(c("project", "project_name") %in% names(results))) {
    results <- dplyr::select(results, -project) %>%
      dplyr::rename(project = project_name)
  }

  if ("transect_len_surveyed" %in% names(results)) {
    results <- dplyr::rename(results, transect_length = transect_len_surveyed)
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
      dplyr::rename(!!list_cols[[i]] := list_name)
    }
  }

  results
}
