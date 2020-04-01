#' Get endpoint from specified MERMAID project(s)
#'
#' @inheritParams mermaid_GET
#' @param project A way to identify the project(s). Can be project IDs (passed as a character vector directly) or projects resulting from \code{\link{mermaid_get_endpoint}} or \code{\link{mermaid_search_projects}}. Defaults to the projects listed via \code{get_default_project}, if available.
#'
#' @export
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("Sharla test", include_test_projects = TRUE)
#' mermaid_get_project_endpoint(test_project, "sites")
#' }
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "project_profiles", "sampleevents", "sites", "beltfishes/obstransectbeltfishes", "beltfishes/sampleunits", "beltfishes/sampleevents"), limit = NULL, url = base_url, token = mermaid_token()) {

  project_id <- as_id(project)
  check_project(project_id)
  endpoint <- match.arg(endpoint, several.ok = TRUE)

  # Construct full endpoints (with project id)
  full_endpoints <- purrr::map(endpoint, ~ paste0("projects/", project_id, "/", .x))

  # Get endpoint results
  endpoints_res <- purrr::map2(endpoint, full_endpoints, get_project_single_endpoint, limit = limit, url = url, token = token, project_id = project_id, project = project)
  names(endpoints_res) <- endpoint

  # Return endpoints
  if (length(endpoints_res) > 1) {
    endpoints_res
  } else {
    endpoints_res[[endpoint]]
  }
}

get_project_single_endpoint <- function(endpoint, full_endpoint, limit = NULL, url = base_url, token = mermaid_token(), project_id, project) {

  res <- mermaid_GET(full_endpoint, limit = limit, url = url, token = token)

  # Combine multiple projects
  if (length(res) > 1) {
    names(res) <- project_id
    res <- rbind_project_endpoints(res)
    res <- add_project_identifiers(res, project)
  } else {
    res <- res[[full_endpoint]]
  }

  res_lookups <- lookup_choices(res, endpoint, url = url)
  construct_project_endpoint_columns(res_lookups, endpoint)
}

check_project <- function(project) {
  if (all(project == "")) {
    stop("Please supply a project to get data from, either via the `project` argument or by using `mermaid_set_default_project()`.", call. = FALSE)
  }
}

construct_project_endpoint_columns <- function(res, endpoint) {
  if (endpoint == "beltfishes/obstransectbeltfishes") {
    return(res)
  }
  if (nrow(res) == 0 || ncol(res) == 0) {
    cols <- mermaid_project_endpoint_columns[[endpoint]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    dplyr::select(res, tidyselect::any_of(c("project_id", "project")), tidyselect::all_of(mermaid_project_endpoint_columns[[endpoint]]))
  }
}

rbind_project_endpoints <- function(x) {
  df_cols <- purrr::map(x, ~ purrr::map(.x, inherits, "data.frame")) %>%
    purrr::transpose() %>%
    purrr::map(~ any(unlist(.x))) %>%
    unlist()
  df_cols <- names(df_cols[df_cols])
  if (length(df_cols) == 0) {
    if ("project_id" %in% names(x)) {
      purrr::map_dfr(x, tibble::as_tibble)
    } else {
      purrr::map_dfr(x, tibble::as_tibble, .id = "project_id")
    }
  } else {
    x_unpack <- purrr::map(x, unpack_df_cols, df_cols = df_cols)

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
    res <- dplyr::select(res, project = project_name, dplyr::everything())
  } else if ("name" %in% names(project)) {
    res <- res %>%
      dplyr::select(-tidyselect::any_of("project")) %>%
      dplyr::left_join(dplyr::select(project, id, project = name), by = c("project_id" = "id")) %>%
      dplyr::select(project, dplyr::everything())
  }

  if (all(c("project", "project_id") %in% names(res))) {
    res <- dplyr::select(res, -project_id)
  }

  res
}
