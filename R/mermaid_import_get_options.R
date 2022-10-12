#' Get field options for MERMAID import
#'
#' Check the options available for importing a given method into MERMAID, either to see what the options are for each field or to check that your data matches the available fields. Returns a list of each field for a given method, whether it's required (in \code{required}), and a list of available choices, if relevant (in \code{choices}).  Optionally, the fields and options can be saved into an Excel file using the \code{save} parameter.
#'
#' @param method Method to get import fields and options for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", or "all" (to get fields for all methods).
#' @param save
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' options <- mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_import_get_options("fishbelt")
#'
#' names(options)
#' # [1] "Site *"                     "Management *"
#' # [3] "Sample date: Year *"        "Sample date: Month *"
#' # [5] "Sample date: Day *"         "Sample time"
#' # [7] "Depth *"                    "Transect number *"
#' # [9] "Transect label"             "Transect length surveyed *"
#' # [11] "Width *"                    "Fish size bin *"
#' # [13] "Reef slope"                 "Visibility"
#' # [15] "Current"                    "Relative depth"
#' # [17] "Tide"                       "Sample unit notes"
#' # [19] "Observer emails *"          "Fish name *"
#' # [21] "Size *"                     "Count *"
#'
#' options[["Reef slope"]][["required"]]
#' # [1] FALSE
#'
#' options[["Reef slope"]][["choices"]]
#' # A tibble: 4 × 1
#' # value
#' # <chr>
#' # 1 crest
#' # 2 flat
#' # 3 slope
#' # 4 wall
#' }
mermaid_import_get_options <- function(project, method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", "all"), save = NULL, token = mermaid_token()) {
  check_project_data_inputs(method, data = "all") # Faking data input to just allow for checking options

  method[method == "bleaching"] <- "bleachingqc"

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
