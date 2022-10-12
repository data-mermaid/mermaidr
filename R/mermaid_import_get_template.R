#' Get template for MERMAID import
#'
#' Get a template of the fields for importing into MERMAID. Used along with \code{\link{mermaid_import_get_options}}, which contains details on which fields are required and their possible options, if relevant. Optionally, the template can be saved into an Excel file using the \code{save} parameter.
#'
#' @param method Method to get import fields for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", or "all" (to get fields for all methods).
#' @param save
#'
#' @return
#' @export
#'
#' @examples
mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity", "all"), save = NULL) {
  check_project_data_inputs(method, data = "all") # Faking data input to just allow for checking options

  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity")
  }

  endpoints <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoints, mermaid_GET)

  if (length(method) == 1) {
    res[[1]][[1]]
  } else {
    res <- purrr::map(res, ~ .x[[1]])
    names(res) <- method
    res[method]
  }
}
