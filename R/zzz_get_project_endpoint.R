#' @param token API token. Authenticate manually via \code{\link{mermaid_auth}}, or automatically when running any project- or user-specific functions (like this one).
#' @param project A way to identify the project(s). Can be project IDs (passed as a character vector directly) or projects resulting from \code{\link{mermaid_get_my_projects}} or \code{\link{mermaid_search_my_projects}}. Defaults to the projects listed via \code{mermaid_get_default_project}, if available.
#'
#' @name get_project_endpoint
NULL

#' Get endpoint from specified MERMAID project(s)
#'
#' @inheritParams mermaid_GET
#' @inheritParams get_project_endpoint
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
#' mermaid_get_project_endpoint(test_project, "sites")
#' }
get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint, limit = NULL, token = mermaid_token(), filter = NULL, covariates = FALSE) {
  project_id <- as_id(project)
  check_project(project_id)

  # Construct full endpoints (with project id)
  full_endpoints <- purrr::map(endpoint, ~ paste0("projects/", project_id, "/", .x))

  # Get endpoint results
  endpoints_res <- purrr::map2(endpoint, full_endpoints, get_project_single_endpoint, limit = limit, filter = filter, token = token, project_id = project_id, project = project, covariates = covariates)
  names(endpoints_res) <- endpoint

  # Return endpoints
  if (length(endpoints_res) == 1) {
    res <- endpoints_res[[endpoint]]
  } else {
    res <- endpoints_res
  }

  # Expand df-cols, only for project_data functions (which have a / in their endpoints)
  if (all(stringr::str_detect(endpoint, "/")) & !any(stringr::str_detect(endpoint, "ingest_schema"))) {
    if (length(endpoint) == 1) {
      if (nrow(res) == 0) {
        dplyr::select(res, tidyselect::any_of(project_data_test_columns[[endpoint]]))
      } else {
        clean_df_cols(res)
      }
    } else {
      if (all(purrr::map_dbl(res, nrow) == 0)) {
        purrr::imap(res, ~ dplyr::select(.x, tidyselect::any_of(project_data_test_columns[[.y]])))
      } else {
        purrr::map(res, clean_df_cols)
      }
    }
  } else {
    res
  }
}

get_project_single_endpoint <- function(endpoint, full_endpoint, limit = NULL, token = mermaid_token(), filter = NULL, project_id, project, covariates = FALSE) {
  initial_res <- mermaid_GET(full_endpoint, limit = limit, filter = filter, token = token)

  # Return ingest schema for tidying separately
  if (stringr::str_detect(endpoint, "ingest_schema")) {
    return(initial_res)
  }

  # Combine multiple projects
  if (length(initial_res) > 1) {
    names(initial_res) <- project_id
    res <- rbind_project_endpoints(initial_res, endpoint)
    res <- add_project_identifiers(res, project)
  } else {
    res <- initial_res[[full_endpoint]]
    res <- dplyr::select(res, -tidyselect::any_of("project"))
  }

  res_lookups <- lookup_choices(res, endpoint, endpoint_type = "project")
  res_strip_suffix <- strip_name_suffix(res_lookups)
  res <- construct_project_endpoint_columns(res_strip_suffix, endpoint, multiple_projects = length(initial_res) > 1, covariates = covariates)

  # Convert any JSON remaining in CSV columns to comma separated list
  if (stringr::str_ends(endpoint, "csv")) {
    json_cols <- res %>%
      head(1) %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::filter(stringr::str_starts(.data$value, "\\[\\{") | stringr::str_starts(.data$value, "\\{"))

    # Treat tags separately
    if ("tags" %in% json_cols[["name"]]) {
      res[["tags"]] <- res[["tags"]] %>%
        stringr::str_replace_all("'", '"') %>%
        purrr::map(function(tags) {
          tags %>%
            jsonlite::fromJSON() %>%
            dplyr::pull(name) %>%
            paste0(collapse = "; ")
        }) %>%
        unlist()
    }

    json_cols <- json_cols %>%
      dplyr::filter(.data$name != "tags")

    if (nrow(json_cols > 0)) {
      # Iterate through columns, unnest etc, widen
      cols <- json_cols[["name"]]
      res <- res %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(cols),
          function(data) {
            data %>%
              stringr::str_replace_all("'", '"') %>%
              purrr::map(jsonlite::fromJSON) %>%
              purrr::transpose() %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              tidyr::unnest(dplyr::everything()) %>%
              dplyr::mutate_all(~ purrr::map(.x, function(y) { # Convert NULL to NA
                ifelse(is.null(y), NA, y)
              }) %>%
                unlist())
          }
        )) %>%
        tidyr::unpack(tidyselect::all_of(cols), names_sep = "_")

      names(res) <- stringr::str_replace_all(names(res), "-", "_")

      # TODO not all same cols
      # TODO handle []
    }

    # Combine sample date year, month, day, into single field, place after management_rules
    if (all(c("sample_date_year", "sample_date_month", "sample_date_day") %in% names(res))) {
      res <- res %>%
        dplyr::mutate(
          sample_date = ISOdate(.data$sample_date_year, .data$sample_date_month, .data$sample_date_day),
          sample_date = as.Date(.data$sample_date)
        ) %>%
        dplyr::relocate(.data$sample_date, .after = .data$management_rules) %>%
        dplyr::select(-.data$sample_date_year, -.data$sample_date_month, -.data$sample_date_day)
    }
  }

  res
}

check_project <- function(project) {
  if (all(project == "")) {
    stop("Please supply a project to get data from, either via the `project` argument or by using `mermaid_set_default_project()`.", call. = FALSE)
  }
}

construct_project_endpoint_columns <- function(res, endpoint, multiple_projects = FALSE, covariates = FALSE) {
  # If covariates = TRUE, add site_id to columns selected (just for now!)
  endpoint_cols <- mermaid_project_endpoint_columns[[endpoint]]

  if (covariates) {
    endpoint_cols <- c("site_id", endpoint_cols)
  }

  if (nrow(res) == 0 && ncol(res) == 0) {
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(endpoint_cols)), .name_repair = "minimal")
    names(res) <- endpoint_cols
    res
  } else if (multiple_projects) {
    dplyr::select(res, tidyselect::any_of(c("project_id", "project")), tidyselect::any_of(endpoint_cols))
  } else {
    dplyr::select(res, dplyr::any_of(endpoint_cols))
  }
}

rbind_project_endpoints <- function(x, endpoint) {
  x <- make_consistent_columns(x)

  df_cols <- purrr::map(x, ~ purrr::map(.x, inherits, "data.frame")) %>%
    purrr::transpose() %>%
    purrr::map(~ any(unlist(.x))) %>%
    unlist()
  df_cols <- names(df_cols[df_cols])

  if (length(df_cols) == 0) {
    if (all(purrr::map_lgl(purrr::map(x, names), ~ "project_id" %in% .x))) {
      x <- purrr::map(purrr::keep(x, ~ nrow(.x) > 0), tibble::as_tibble)
      do.call("rbind", x)
    } else {
      x <- purrr::map(purrr::keep(x, ~ nrow(.x) > 0), tibble::as_tibble)
      x <- purrr::imap(x, function(x, name) x %>% dplyr::mutate(project_id = name))
      do.call("rbind", x)
    }
  } else {
    x_unpack <- purrr::map(x, unpack_df_cols, df_cols = df_cols)
    x_unpack <- purrr::keep(x_unpack, ~ nrow(.x) > 0)

    if (all(unlist(purrr::map(x_unpack, ~ "project_id" %in% names(.x))))) {
      x_rbind <- do.call("rbind", x_unpack)
    } else {
      x_rbind <- purrr::imap(x_unpack, function(x, name) x %>% dplyr::mutate(project_id = name))
      x_rbind <- do.call("rbind", x_rbind)
    }

    attr(x_rbind, "df_cols") <- attr(x_unpack[[1]], "df_cols")
    attr(x_rbind, "col_order") <- c("project_id", attr(x_unpack[[1]], "col_order"))
    repack_df_cols(x_rbind)
  }
}

make_consistent_columns <- function(x) {
  res_names <- purrr::map(x, names)
  res_names_length <- purrr::map_dbl(res_names, length)
  res_lengths <- unname(res_names_length)

  if (all(res_lengths == 0) | all(res_lengths > 0)) {
    return(x)
  }

  res_empty <- names(res_names_length[res_names_length == 0])
  res_longest <- res_names_length[res_names_length == max(res_names_length)][1]

  x[res_empty] <- x[res_empty] %>%
    purrr::map(~ {
      .x <- tibble::as_tibble(matrix(nrow = 0, ncol = unname(res_longest)), .name_repair = "minimal")
      names(.x) <- res_names[names(res_longest)][[names(res_longest)]]
      return(.x)
    })

  x
}

unpack_df_cols <- function(x, df_cols = NULL) {
  if (is.null(df_cols)) {
    df_cols <- sapply(x, inherits, "data.frame")
    df_cols <- names(df_cols[df_cols])
  }

  if (all(sapply(x[df_cols], inherits, "data.frame"))) {
    x_unpack <- x %>%
      tidyr::unpack(
        cols = tidyselect::all_of(df_cols),
        names_sep = "_"
      )
  } else {
    x_unpack <- x %>%
      dplyr::select(-tidyselect::any_of(df_cols))
  }

  attr(x_unpack, "df_cols") <- df_cols
  attr(x_unpack, "col_order") <- names(x)

  x_unpack
}

repack_df_cols <- function(x) {
  df_cols <- attr(x, "df_cols")
  col_order <- attr(x, "col_order")

  for (i in seq_along(df_cols)) {
    x <- x %>%
      tidyr::pack(!!df_cols[[i]] := dplyr::starts_with(paste0(df_cols[[i]], "_")))

    x[[df_cols[[i]]]] <- x[[df_cols[[i]]]] %>%
      dplyr::rename_all(~ gsub(paste0("^", df_cols[[i]], "_"), "", .x))
  }

  dplyr::select(x, tidyselect::all_of(col_order))
}

add_project_identifiers <- function(res, project) {
  if (ncol(res) == 0) {
    return(res)
  }

  if ("project_name" %in% names(res)) {
    res <- dplyr::select(res, tidyselect::all_of(c(project = "project_name")), dplyr::everything())
  } else if ("name" %in% names(project)) {
    res <- res %>%
      dplyr::select(-tidyselect::any_of("project")) %>%
      dplyr::left_join(dplyr::select(project, tidyselect::all_of(c("id", project = "name"))), by = c("project_id" = "id")) %>%
      dplyr::select(project, dplyr::everything())
  }

  if (all(c("project", "project_id") %in% names(res))) {
    if (all(res[["project"]] == res[["project_id"]])) {
      res <- dplyr::select(res, -tidyselect::all_of("project"))
    } else {
      res <- dplyr::select(res, -tidyselect::all_of("project_id"))
    }
  }

  res
}

clean_df_cols <- function(.data) {
  df_cols <- sapply(.data, function(x) inherits(x, "data.frame"))
  df_cols <- names(df_cols[df_cols])

  .data %>%
    tidyr::unpack(cols = dplyr::all_of(df_cols), names_sep = "_") %>%
    dplyr::rename_all(~ .x %>%
      stringr::str_replace_all(" |-", "_") %>%
      stringr::str_to_lower())
}

mermaid_project_endpoint_columns <- list(
  managements = project_managements_columns,
  sites = project_sites_columns
)

mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_other_endpoint_columns)

mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_data_columns)
