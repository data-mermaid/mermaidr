#' Get endpoint from a specified MERMAID project
#'
#' @inheritParams mermaid_GET
#' @param project A way to identify the project. Can be a project ID (passed as a character vector directly) or a single project resulting from \code{\link{mermaid_get_endpoint}} or \code{\link{mermaid_search_projects}}. Defaults to the project listed via \code{get_default_project}, if available.
#'
#' @export
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("Sharla test")
#' mermaid_get_project_endpoint(test_project, "sites")
#' }
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "profiles", "project_profiles", "sampleevents", "sites", "beltfishes/obstransectbeltfishes/", "beltfishes/sampleunits/", "beltfishes/sampleevents/"), limit = NULL, url = base_url, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)
  endpoint <- match.arg(endpoint)

  full_endpoint <- paste0("projects/", project_id, "/", endpoint)
  res <- mermaid_GET(full_endpoint, limit = limit, url = url, token = token)

  # Clean up results
  if (inherits(res, "list")) {
    clean_res <- purrr::map2(res, endpoint, clean_project_endpoint)
    names(clean_res) <- project_id
    clean_res_rbind <- rbind_project_endpoints(clean_res)

    if (all(c("name", "id") %in% names(project)) && !all(c("project_name", "project_id") %in% names(clean_res_rbind))) {
      clean_res_rbind %>%
        dplyr::left_join(project %>%
          dplyr::select(id, project_name = name), by = c("project_id" = "id")) %>%
        dplyr::select(project_id, project_name, dplyr::everything())
    } else {
      clean_res_rbind
    }
  } else {
    clean_project_endpoint(res, endpoint)
  }
}

check_project <- function(project) {
  if (all(project == "")) {
    stop("Please supply a project to get data from, either via the `project` argument or by using `mermaid_set_default_project()`.", call. = FALSE)
  }
}

new_endpoints <- c("beltfishes/obstransectbeltfishes/", "beltfishes/sampleunits/", "beltfishes/sampleevents/")

clean_project_endpoint <- function(res, endpoint) {
  if(endpoint == "beltfishes/obstransectbeltfishes/") {
    return(res)
  }
  if (nrow(res) == 0 || ncol(res) == 0) {
    cols <- mermaid_endpoint_columns[[ifelse(endpoint == "managements", "managements_project", endpoint)]]
    res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
    names(res) <- cols
    res
  } else {
    res[, mermaid_endpoint_columns[[ifelse(endpoint == "managements", "managements_project", endpoint)]]]
  }
}

rbind_project_endpoints <- function(x) {
  df_cols <- sapply(x, inherits, "data.frame")
  df_cols <- names(df_cols[df_cols])
  if (length(df_cols) == 0) {
    if ("project_id" %in% names(x)) {
      purrr::map_dfr(x, tibble::as_tibble)
    } else {
      purrr::map_dfr(x, tibble::as_tibble, .id = "project_id")
    }
  } else {
    x_unpack <- purrr::map(x, unpack_df_cols)

    if (all(unlist(purrr::map(x_unpack, ~ "project_id" %in% names(.x))))) {
      x_rbind <- purrr::map_dfr(x_unpack, tibble::as_tibble)
    } else {
      x_rbind <- purrr::map_dfr(x_unpack, tibble::as_tibble, .id = "project_id")
    }

    attr(x_rbind, "df_cols") <- attr(x_unpack[[1]], "df_cols")
    attr(x_rbind, "col_order") <- c("project_id", attr(x_unpack[[1]], "col_order"))
    repack_df_cols(x_rbind)
  }
}

unpack_df_cols <- function(x) {
  df_cols <- sapply(x, inherits, "data.frame")
  df_cols <- names(df_cols[df_cols])

  x_unpack <- x %>%
    tidyr::unpack(cols = df_cols,
                  names_sep = "_")

  attr(x_unpack, "df_cols") <- df_cols
  attr(x_unpack, "col_order") <- names(x)

  x_unpack
}

repack_df_cols <- function(x) {
  df_cols <- attr(x, "df_cols")
  col_order <- attr(x, "col_order")

  for(i in seq_along(df_cols)) {
    x <- x %>%
      tidyr::pack(!!df_cols[[i]] := dplyr::starts_with(paste0(df_cols[[i]], "_")))

    x[[df_cols[[i]]]] <- x[[df_cols[[i]]]] %>%
      dplyr::rename_all(~ gsub(paste0("^", df_cols[[i]], "_"), "", .x))
  }

  dplyr::select(x, tidyselect::all_of(col_order))
}
