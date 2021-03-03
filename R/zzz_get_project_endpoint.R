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
get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "project_profiles", "sampleevents", "sites", "beltfishes/obstransectbeltfishes", "beltfishes/sampleunits", "beltfishes/sampleevents", "benthicpits/obstransectbenthicpits", "benthicpits/sampleunits", "benthicpits/sampleevents", "benthiclits/obstransectbenthiclits", "benthiclits/sampleunits", "benthiclits/sampleevents", "habitatcomplexities/obshabitatcomplexities", "habitatcomplexities/sampleunits", "habitatcomplexities/sampleevents", "bleachingqcs/obscoloniesbleacheds", "bleachingqcs/obsquadratbenthicpercents", "bleachingqcs/sampleunits", "bleachingqcs/sampleevents"), limit = NULL, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)
  endpoint <- match.arg(endpoint, several.ok = TRUE)

  # Construct full endpoints (with project id)
  full_endpoints <- purrr::map(endpoint, ~ paste0("projects/", project_id, "/", .x))

  # Get endpoint results
  endpoints_res <- purrr::map2(endpoint, full_endpoints, get_project_single_endpoint, limit = limit, token = token, project_id = project_id, project = project)
  names(endpoints_res) <- endpoint

  # Return endpoints
  if (length(endpoints_res) == 1) {
    res <- endpoints_res[[endpoint]]
  } else {
    res <- endpoints_res
  }

  # Expand df-cols, only for project_data functions (which have a / in their endpoints)
  if (all(stringr::str_detect(endpoint, "/"))) {
    if (length(endpoint) == 1) {
      if(nrow(res) == 0) {
        dplyr::select(res, project_data_test_columns[[endpoint]])
      } else {
        clean_df_cols(res)
      }
    } else {
      if (all(purrr::map_dbl(res, nrow) == 0)) {
        purrr::imap(res, ~ dplyr::select(.x, project_data_test_columns[[.y]]))
      } else {
        purrr::map(res, clean_df_cols)
      }
    }
  } else {
    res
  }
}

get_project_single_endpoint <- function(endpoint, full_endpoint, limit = NULL, token = mermaid_token(), project_id, project) {
  initial_res <- mermaid_GET(full_endpoint, limit = limit, token = token)

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
  construct_project_endpoint_columns(res_strip_suffix, endpoint, multiple_projects = length(initial_res) > 1)
}

check_project <- function(project) {
  if (all(project == "")) {
    stop("Please supply a project to get data from, either via the `project` argument or by using `mermaid_set_default_project()`.", call. = FALSE)
  }
}

construct_project_endpoint_columns <- function(res, endpoint, multiple_projects = FALSE) {
  if (nrow(res) == 0 && ncol(res) == 0) {
    cols <- mermaid_project_endpoint_columns[[endpoint]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else if (multiple_projects) {
    dplyr::select(res, tidyselect::any_of(c("project_id", "project")), mermaid_project_endpoint_columns[[endpoint]])
  } else {
    dplyr::select(res, mermaid_project_endpoint_columns[[endpoint]])
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
      purrr::map_dfr(purrr::keep(x, ~ nrow(.x) > 0), tibble::as_tibble)
    } else {
      purrr::map_dfr(purrr::keep(x, ~ nrow(.x) > 0), tibble::as_tibble, .id = "project_id")
    }
  } else {
    x_unpack <- purrr::map(x, unpack_df_cols, df_cols = df_cols)
    x_unpack <- purrr::keep(x_unpack, ~ nrow(.x) > 0)

    if (all(unlist(purrr::map(x_unpack, ~ "project_id" %in% names(.x))))) {
      x_rbind <- dplyr::bind_rows(x_unpack)
    } else {
      x_rbind <- dplyr::bind_rows(x_unpack, .id = "project_id")
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
        cols = df_cols,
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
  if ("project_name" %in% names(res)) {
    res <- dplyr::select(res, project = .data$project_name, dplyr::everything())
  } else if ("name" %in% names(project)) {
    res <- res %>%
      dplyr::select(-tidyselect::any_of("project")) %>%
      dplyr::left_join(dplyr::select(project, .data$id, project = .data$name), by = c("project_id" = "id")) %>%
      dplyr::select(project, dplyr::everything())
  }

  if (all(c("project", "project_id") %in% names(res))) {
    if (all(res[["project"]] == res[["project_id"]])) {
      res <- dplyr::select(res, -.data$project)
    } else {
      res <- dplyr::select(res, -.data$project_id)
    }
  }

  res
}

clean_df_cols <- function(.data) {
  df_cols <- sapply(.data, function(x) inherits(x, "data.frame"))
  df_cols <- names(df_cols[df_cols])

  .data %>%
    tidyr::unpack(cols = dplyr::all_of(df_cols), names_sep = "_") %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "_by") %>%
                        stringr::str_replace_all(" |-", "_") %>%
                        stringr::str_to_lower())
}

mermaid_project_endpoint_columns <- list(
  managements = project_managements_columns,
  sites = project_sites_columns
)

mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_other_endpoint_columns)

mermaid_project_endpoint_columns <- append(mermaid_project_endpoint_columns, project_data_columns)
