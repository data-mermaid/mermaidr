#' Get MERMAID reference
#'
#' Find the names and information of the fish and benthic attributes you can choose in MERMAID.
#'
#' @param reference MERMAID reference. One of "fishfamilies", "fishgenera", "fishspecies", "benthicattributes", "invertattributes", "invertspecies".
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_reference("benthicattributes")
#' mermaid_get_reference(c("fishfamilies", "fishgenera"))
#' }
mermaid_get_reference <- function(reference = c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes", "invertattributes", "invertspecies"), limit = NULL) {
  if (!all(reference %in% references_list)) {
    stop(paste("`reference` must be one of:", comma_sep(references_list)), call. = FALSE)
  }

  reference <- match.arg(reference, several.ok = TRUE)

  choices <- mermaid_get_endpoint("choices")
  res <- purrr::map(reference, get_single_reference, limit, choices)
  names(res) <- reference
  res <- purrr::imap(
    res,
    function(res, name) {
      if (name %in% c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes")) {
        lookup_regions(res, choices)
      } else {
        res
      }
    }
  )

  if (length(reference) > 1) {
    names(res) <- reference
    res
  } else {
    res[[1]]
  }
}

references_list <- c(
  "fishfamilies", "fishgenera", "fishspecies", "benthicattributes", "invertattributes", "invertspecies"
)

get_single_reference <- function(reference, limit = NULL, choices = mermaid_get_endpoint("choices")) {
  switch(reference,
    fishfamilies = get_endpoint("fishfamilies", limit = limit),
    fishgenera = get_reference_fishgenera(limit = limit),
    fishspecies = get_reference_fishspecies(limit = limit, choices = choices),
    benthicattributes = get_reference_benthicattributes(limit = limit, choices = choices),
    invertattributes = get_reference_invertattributes(limit = limit, choices = choices),
    invertspecies = get_reference_invertspecies(limit = limit, choices = choices)
  ) %>%
    remove_blacklist_endpoint_columns(reference, nested = "reference")
}

get_reference_fishgenera <- function(limit = NULL) {
  fishgenera <- get_endpoint("fishgenera", limit = limit)
  fishfamilies <- get_endpoint("fishfamilies") %>%
    dplyr::select(tidyselect::all_of(c("id", family = "name")))

  fishgenera %>%
    dplyr::left_join(fishfamilies, by = c("family" = "id"), suffix = c("_id", ""))
}

get_reference_fishspecies <- function(limit = NULL, choices = mermaid_get_endpoint("choices")) {
  fishspecies <- get_endpoint("fishspecies", limit = limit)

  fishgenera <- get_endpoint("fishgenera")

  choices <- choices %>%
    tibble::deframe()

  fishgroupsizes <- choices[["fishgroupsizes"]] %>%
    dplyr::select(tidyselect::all_of(c("id", group_size = "name")))

  fishgrouptrophics <- choices[["fishgrouptrophics"]] %>%
    dplyr::select(tidyselect::all_of(c("id", trophic_group = "name")))

  fishgroupfunctions <- choices[["fishgroupfunctions"]] %>%
    dplyr::select(tidyselect::all_of(c("id", functional_group = "name")))

  genus <- fishgenera %>%
    dplyr::select(tidyselect::all_of(c("id", genus = "name")))

  fishspecies %>%
    dplyr::left_join(genus, by = c("genus" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgroupsizes, by = c("group_size" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgrouptrophics, by = c("trophic_group" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(fishgroupfunctions, by = c("functional_group" = "id"), suffix = c("_id", ""))
}

get_reference_benthicattributes <- function(limit = NULL, choices = mermaid_get_endpoint("choices")) {
  benthicattributes <- get_endpoint("benthicattributes", limit = limit)

  # Lookup life histories
  res <- benthicattributes %>%
    lookup_benthiclifehistories(choices)

  benthicattributes %>%
    dplyr::left_join(benthicattributes %>%
      dplyr::select(tidyselect::all_of(c(parent_id = "id", parent = "name"))), by = c("parent" = "parent_id"), suffix = c("_id", "")) %>%
    dplyr::left_join(benthicattributes %>%
      dplyr::select(tidyselect::all_of(c(top_level_id = "id", top_level_category = "name"))), by = c("top_level_category" = "top_level_id"), suffix = c("_id", ""))
}

get_reference_invertattributes <- function(limit = NULL, choices = mermaid_get_endpoint("choices")) {
  invertattributes <- get_endpoint("invertattributes", limit = limit)

  choices <- choices %>%
    tibble::deframe()

  invertgroupsofinterest <- choices[["invertgroupsofinterest"]] %>%
    dplyr::select(tidyselect::all_of(c("id", group_of_interest = "name")))

  invertparents <- invertattributes %>%
    dplyr::select(tidyselect::all_of(c("id", parent = "name")))

  invertattributes %>%
    dplyr::left_join(invertgroupsofinterest, by = c("group_of_interest" = "id"), suffix = c("_id", "")) %>%
    dplyr::left_join(invertparents, by = c("parent" = "id"), suffix = c("_id", ""))
}

get_reference_invertspecies <- function(limit = NULL, choices = mermaid_get_endpoint("choices")) {
  invertspecies <- get_endpoint("invertspecies", limit = limit)

  # Lookup genus
  invertattributes <- get_reference_invertattributes(choices = choices)

  invertgenus <- invertattributes %>%
    dplyr::filter(.data$taxonomic_rank == "genus") %>%
    dplyr::select(tidyselect::all_of(c("id", genus = "name")))

  invertspecies %>%
    dplyr::left_join(invertgenus, by = c("genus" = "id"), suffix = c("_id", ""))
}

lookup_regions <- function(results, choices = mermaid_get_endpoint("choices")) {
  regions <- choices %>%
    tibble::deframe() %>%
    purrr::pluck("regions") %>%
    dplyr::select(tidyselect::all_of(c("id", regions = "name")))

  results_row <- results %>%
    dplyr::mutate(row = dplyr::row_number())

  row_regions <- results_row %>%
    dplyr::select(tidyselect::all_of(c("row", "regions"))) %>%
    tidyr::separate_rows("regions", sep = ", ") %>%
    dplyr::filter(!is.na(.data$regions)) %>%
    dplyr::left_join(regions, by = c("regions" = "id"), suffix = c("_id", "")) %>%
    dplyr::group_by(.data$row) %>%
    dplyr::arrange(.data$regions) %>%
    dplyr::summarise(regions = paste(.data$regions, collapse = ", "))

  results_row %>%
    dplyr::left_join(row_regions, by = "row", suffix = c("_id", "")) %>%
    dplyr::select(-tidyselect::all_of(c("row", "regions_id"))) %>%
    dplyr::select(names(results))
}

lookup_benthiclifehistories <- function(results, choices = mermaid_get_endpoint("choices")) {
  life_histories <- choices %>%
    tibble::deframe() %>%
    purrr::pluck("benthiclifehistories") %>%
    dplyr::select(tidyselect::all_of(c("id", "name")))

  results_row <- results %>%
    dplyr::mutate(row = dplyr::row_number())

  row_lifehistories <- results_row %>%
    dplyr::select(tidyselect::all_of(c("row", "id" = "life_histories"))) %>%
    tidyr::separate_rows("id", sep = ", ") %>%
    dplyr::filter(!is.na(.data$id)) %>%
    dplyr::left_join(life_histories, by = "id", suffix = c("_id", "")) %>%
    dplyr::group_by(.data$row) %>%
    dplyr::arrange(.data$id) %>%
    dplyr::summarise(id = paste(.data$name, collapse = ", "))

  names(row_lifehistories) <- c("row", "life_histories")

  results_row %>%
    dplyr::left_join(row_lifehistories, by = "row", suffix = c("_id", "")) %>%
    dplyr::select(-tidyselect::all_of(c("row", "life_histories_id"))) %>%
    dplyr::select(names(results))
}
