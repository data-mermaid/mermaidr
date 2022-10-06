mermaid_import_get_options <- function(project, method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity", "all"), token = mermaid_token()) {
  check_project_data_inputs(method, data = "all") # Faking data input to just allow for checking options

  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity")
  }

  endpoint <- glue::glue("collectrecords/ingest_schema/{method}")

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit = NULL, token))

  res <- purrr::map(res, clean_import_options)

  if (length(method) == 1) {
    res[[1]]
  } else {
    names(res) <- method
    res[method]
  }
}

clean_import_options <- function(data) {
  data <- data[[1]]

  # Name each list element with "label"
  labels <- data %>% purrr::map_chr("label")
  names(data) <- labels

  # Remove "label" field, "help_text" (all empty right now), and "name"
  data <- data %>%
    purrr::map(function(x) {
      x[["label"]] <- NULL
      x[["help_text"]] <- NULL
      x[["name"]] <- NULL
      x
    })

  # Convert "choices" to a tibble (to take advantage of print options - don't want to print 1000s)
  # If it is a 0 row tibble, just remove it
  data <- data %>%
    purrr::map(function(x) {
      x[["choices"]] <- dplyr::as_tibble(x[["choices"]], column_name = "choice")

      if (nrow(x[["choices"]]) == 0) {
        x[["choices"]] <- NULL
      }

      x
    })
}
