mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity", "all")) {
  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity")
  }

  endpoints <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoints, mermaid_GET)

  if (length(method) == 1) {
    res[[1]]
  } else {
    res <- purrr::map(res, ~ .x[[1]])
    names(res) <- method
    res[method]
  }
}
