#' Get MERMAID project data
#'
#' Get Fish Belt, Benthic LIT, Benthic PIT, Macroinvertebrate, Bleaching, or Habitat Complexity data for your MERMAID projects. Data is available at the observation, sample unit, and sample event level. Optionally get covariates at the site level. See Details section for more. Requires authorization.
#'
#' Fish Belt method data is available by setting \code{method} to "fishbelt". Fish Belt observations data contains individual observations recorded in MERMAID, while sample units contains total biomass in kg/ha per sample unit, by trophic group and by fish family. Sample events data contains \emph{mean} total biomass in kg/ha per sample event, and by trophic group and by fish family, as well as the  \emph{standard deviation} of the total biomass as well as per trophic group and fish family.
#'
#' Benthic LIT method data is available by setting \code{method} to "benthiclit". Benthic LIT observations contain individual observations. Sample units data returns percent cover per sample unit, by benthic category. Sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.
#'
#' Benthic PIT method data is available by setting \code{method} to "benthicpit". Similarly to Benthic LIT, Benthic PIT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category, and sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.
#'
#' Benthic Photo Quadrat method data is available by setting \code{method} to "benthicpqt". Benthic PQT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category and by life histories, and sample events contain \emph{mean} percent cover per sample event, by benthic category and by life histories, and standard deviations of these values.
#'
#' Macroinvertebrate data is available by setting \code{method} to "macroinvertebrate". Observations contain individual observations, with a full breakdown of macroinvertebrate class, order, family, genus, and group of interest, along with the size, count, and density. Sample units data returns total abundance and density, as well as density by group of interest, and sample events contain \emph{mean} density per sample event, by group of interest, and standard deviations of these values.
#'
#' Bleaching method data is available by setting \code{method} to "bleaching". When Bleaching observations are requested, two types of observations are returned: Colonies Bleached and Percent Cover. Sample units data contains both Colonies Bleached data (number of coral genera and total number of colonies, and percent normal, pale, and bleached colonies) and Percent Cover data (Number of quadrats, and average percent cover for hard coral, soft coral, and macroalgae), all per sample unit. Sample events data contains \emph{mean} values of all the data in sample units, for both Colonies Bleached (average quadrat size, average number of coral genera and average total colonies, average percent normal, pale, and bleached colonies) and Percent Cover (average number of quadrats, and average of average hard coral, soft coral, and macroalgae cover). Mean percent cover values also come with standard deviations.
#'
#' Habitat Complexity data is available by setting \code{method} to "habitatcomplexity". Observations contain individual observations, with the habitat complexity score at each interval. Sample units data contains the average habitat complexity score for the sample unit, and sample events data contains the average of those averages habitat complexity scores, along with standard deviations.
#'
#' The included covariates are: geomorphic zonation and benthic habitat from the \href{https://allencoralatlas.org}{Allen Coral Atlas}; market gravity, water pollution (sediments and nitrogen), coastal population, industrial development (number of ports), tourism (reef value), and a cumulative pressure index from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12858}{\emph{A global map of human pressures on tropical coral reefs}} by Andrello et al., 2022; scores from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12587}{\emph{Risk-sensitive planning for conserving coral reefs under rapid climate change}} by Beyer et al., 2018.
#'
#' @param method Method to get data for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", bleaching", "habitatcomplexity", "macroinvertebrate", or "all" (to get data for all methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or "all" (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @inheritParams mermaid_get_project_sites
#' @param covariates Deprecated as of version \code{2.0.0}. Please use `mermaidrcovariates` package to access covariates.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_data(method = "fishbelt", data = "observations", limit = 10)
#'
#' projects %>%
#'   mermaid_get_project_data(method = c("benthicpit", "fishbelt"), data = "sampleevents", limit = 10)
#'
#' bleaching_obs <- projects %>%
#'   mermaid_get_project_data(method = "bleaching", data = "observations", limit = 10)
#' names(bleaching_obs)
#' # [1] "colonies_bleached" "percent_cover"
#' }
mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "macroinvertebrate", "all"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, token = mermaid_token(), covariates = FALSE) {
  internal_mermaid_get_project_data(project, method, data, limit, covariates = covariates, token)
}

internal_mermaid_get_project_data <- function(project = mermaid_get_default_project(), method = methods_all, data = data_types_all, limit = NULL, covariates = FALSE, token = mermaid_token()) {
  check_project_data_inputs(method, data)

  if (any(method == "all")) {
    method <- methods
  }
  if (any(data == "all")) {
    data <- data_types
  }

  endpoint <- construct_endpoint(method, data)

  res <- purrr::map(endpoint, function(x) get_project_endpoint(project, x, limit, token, covariates = covariates))

  if (all(purrr::map_lgl(res, inherits, "list"))) {
    res <- purrr::map(res, ~ {
      names(.x) <- data
      .x
    })
  }

  if (any(method == "bleaching")) {
    if (all(data == "observations")) {
      names(res[["bleachingqcs"]]) <- c("colonies_bleached", "percent_cover")
    } else if ("observations" %in% data) {
      names(res[["bleachingqcs"]]) <- sub_one_for_many(x = data, pattern = "observations", replacement = c("colonies_bleached", "percent_cover"))
      res[["bleachingqcs"]][["observations"]] <- list(colonies_bleached = res[["bleachingqcs"]][["colonies_bleached"]], percent_cover = res[["bleachingqcs"]][["percent_cover"]])
      res[["bleachingqcs"]] <- res[["bleachingqcs"]][data]
    }
  }

  if (length(endpoint) == 1) {
    res[[1]]
  } else {
    names(res) <- method
    res
  }
}

check_project_data_inputs <- function(method, data) {
  if (!all(method %in% methods_all)) {
    stop(methods_plus_all_err, call. = FALSE)
  }
  if (!all(data %in% data_types_all)) {
    stop("`data` must be one of: ", comma_sep(data_types_all), call. = FALSE)
  }
}

construct_endpoint <- function(method, data) {
  method_data <- tidyr::expand_grid(method = method, data = data)

  method_data <- method_data %>%
    dplyr::mutate(
      method = dplyr::case_when(
        .data$method == "fishbelt" ~ "beltfishes",
        .data$method == "benthicpit" ~ "benthicpits",
        .data$method == "benthiclit" ~ "benthiclits",
        .data$method == "benthicpqt" ~ "benthicpqts",
        .data$method == "habitatcomplexity" ~ "habitatcomplexities",
        .data$method == "bleaching" ~ "bleachingqcs",
        .data$method == "macroinvertebrate" ~ "beltinverts"
      ),
      data = dplyr::case_when(
        .data$data == "observations" & .data$method == "bleachingqcs" ~ "obscoloniesbleacheds,obsquadratbenthicpercents",
        .data$data == "observations" & .data$method == "habitatcomplexities" ~ "obshabitatcomplexities",
        .data$data == "observations" ~ glue::glue("obstransect{.data$method}"),
        TRUE ~ data
      )
    ) %>%
    tidyr::separate_rows(method, data, sep = ",")

  csv_endpoint <- "/csv"

  method_data <- method_data %>%
    dplyr::mutate(endpoint = paste0(.data$method, "/", .data$data, csv_endpoint))

  method_data_list <- method_data %>%
    split(method_data$method) %>%
    purrr::map(dplyr::pull, .data$endpoint)

  method_data_list[unique(method_data[["method"]])]
}
