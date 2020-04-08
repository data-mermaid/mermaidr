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
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "project_profiles", "sampleevents", "sites", "beltfishes/obstransectbeltfishes", "beltfishes/sampleunits", "beltfishes/sampleevents", "benthicpits/obstransectbenthicpits", "benthicpits/sampleunits", "benthicpits/sampleevents"), limit = NULL, url = base_url, token = mermaid_token()) {
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
  initial_res <- mermaid_GET(full_endpoint, limit = limit, url = url, token = token)

  # Combine multiple projects
  if (length(initial_res) > 1) {
    names(initial_res) <- project_id
    res <- rbind_project_endpoints(initial_res, endpoint)
    res <- add_project_identifiers(res, project)
  } else {
    res <- initial_res[[full_endpoint]]
    res <- dplyr::select(res, -tidyselect::any_of("project"))
  }

  res_lookups <- lookup_choices(res, endpoint, url = url, endpoint_type = "project")
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
    res <- dplyr::select(res, project = project_name, dplyr::everything())
  } else if ("name" %in% names(project)) {
    res <- res %>%
      dplyr::select(-tidyselect::any_of("project")) %>%
      dplyr::left_join(dplyr::select(project, id, project = name), by = c("project_id" = "id")) %>%
      dplyr::select(project, dplyr::everything())
  }

  if (all(c("project", "project_id") %in% names(res))) {
    if (all(res[["project"]] == res[["project_id"]])) {
      res <- dplyr::select(res, -project)
    } else {
      res <- dplyr::select(res, -project_id)
    }
  }

  res
}

mermaid_project_endpoint_columns <- list(
  beltfishtransectmethods = c("id", "transect", "sample_event", "fishbelt_transect", "observers", "obs_belt_fishes", "created_on", "updated_on"),
  beltfishes = c("id", "transect", "created_on", "updated_on"),
  benthiclittransectmethods = c("id", "transect", "sample_event", "benthic_transect", "observers", "obs_benthic_lits", "created_on", "updated_on"),
  benthicpittransectmethods = c("id", "transect", "interval_size", "sample_event", "benthic_transect", "observers", "obs_benthic_pits", "created_on", "updated_on"),
  benthicpits = c("id", "transect", "interval_size", "created_on", "updated_on"),
  collectrecords = c("id", "profile", "stage", "data", "validations", "created_on", "updated_on"),
  fishbelttransects = c("id", "notes", "number", "len_surveyed", "reef_slope", "width", "size_bin", "sample_event", "created_on", "updated_on"),
  habitatcomplexities = c("id", "transect", "interval_size", "created_on", "updated_on"),
  obsbenthiclits = c("id", "length", "notes", "benthiclit", "attribute", "growth_form", "created_on", "updated_on"),
  obsbenthicpits = c("id", "data", "interval", "include", "notes", "benthicpit", "attribute", "growth_form", "created_on", "updated_on"),
  obshabitatcomplexities = c("id", "data", "interval", "include", "notes", "habitatcomplexity", "score", "created_on", "updated_on"),
  obstransectbeltfishs = c("id", "data", "size", "count", "include", "notes", "beltfish", "fish_attribute", "size_bin", "created_on", "updated_on"),
  managements = c("id", "name", "name_secondary", "notes", "est_year", "no_take", "periodic_closure", "open_access", "size_limits", "gear_restriction", "species_restriction", "compliance", "predecessor", "parties", "created_on", "updated_on"),
  observers = c("id", "profile", "rank", "transectmethod", "created_on", "created_by"),
  project_profiles = c("id", "profile", "is_collector", "is_admin", "role", "created_on", "updated_on"),
  sampleevents = c("id", "depth", "data", "sample_date", "sample_time", "notes", "created_by", "site", "management", "visibility", "current", "relative_depth", "tide", "created_on", "updated_on"),
  sites = c("id", "name", "notes", "latitude", "longitude", "country", "reef_type", "reef_zone", "exposure", "predecessor", "created_on", "updated_on"),
  `beltfishes/obstransectbeltfishes` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "transect_length", "transect_width", "size_bin", "observers", "depth", "transect_number", "label", "fish_family", "fish_genus", "fish_taxon", "size", "biomass_constant_a", "biomass_constant_b", "biomass_constant_c", "biomass_kgha", "trophic_level", "functional_group", "vulnerability", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "contact_link"),
  `beltfishes/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "size_bin", "transect_length", "transect_width", "biomass_kgha", "biomass_kgha_by_trophic_group", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "id", "contact_link"),
  `beltfishes/sampleevents` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth_avg", "biomass_kgha_avg", "biomass_kgha_by_trophic_group_avg", "data_policy_beltfish", "project_notes", "site_notes", "management_notes", "sample_unit_count", "contact_link"),
  `benthicpits/obstransectbenthicpits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "sample_time", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "label", "observers", "interval", "benthic_category", "benthic_attribute", "growth_form", "data_policy_benthicpit", "project_notes", "site_notes", "management_notes", "observation_notes", "contact_link"),
  `benthicpits/sampleunits` = c("project", "tags", "country", "site", "latitude", "longitude", "reef_type", "reef_zone", "reef_exposure", "reef_slope", "tide", "current", "visibility", "management", "management_secondary", "management_est_year", "management_size", "management_parties", "management_compliance", "management_rules", "sample_date", "depth", "transect_number", "transect_length", "interval_start", "interval_size", "observers", "percent_avgs", "project_notes", "site_notes", "management_notes", "id", "contact_link")
)

