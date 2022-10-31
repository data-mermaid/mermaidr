#' Get template for MERMAID import
#'
#' Get a template of the fields for importing into MERMAID. Used along with \code{\link{mermaid_import_get_options}}, which contains details on which fields are required and their possible options, if relevant. Optionally, the template can be saved into an Excel file using the \code{save} parameter.
#'
#' @param method Method to get import template for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", or "habitatcomplexity".
#' @param save Excel file to save template to - .xlsx or .xls or .csv file. Optional.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_import_get_template("fishbelt")
#' }
mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"), save) {
  check_import_inputs(method)

  if (method == "bleaching") {
    method <- "bleachingqc"
  }

  endpoints <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoints, mermaid_GET)

  res <- res[[1]][[1]]

  if (!missing(save)) {
    # Check that file is xlsx or xls or csv
    check_excel_file(save, csv = TRUE)

    if (!stringr::str_ends(save, "\\.csv")) {

      # Create workbook
      wb <- openxlsx::createWorkbook()

      # Save to a sheet
      openxlsx::addWorksheet(wb, method)

      # Set to full width for each column
      openxlsx::setColWidths(wb, method, 1:ncol(res), nchar(names(res)) + 2) # Buffer

      openxlsx::writeData(wb, method, res)

      # Write workbook
      openxlsx::saveWorkbook(wb, save, overwrite = TRUE)
    } else {
      readr::write_csv(res, save)
    }

    usethis::ui_done("Import template written to {save}")
  }

  res
}

check_import_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching")) | length(method) > 1) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"', call. = FALSE)
  }
}

check_excel_file <- function(save, csv = FALSE) {
  pos <- regexpr("\\.([[:alnum:]]+)$", save)
  filetype <- substring(save, pos + 1L)

  options <- c("xlsx", "xls")

  if (csv) {
    options <- c(options, "csv")
  }

  if (!tolower(filetype) %in% options | pos <= 1) { # pos checks if it is just e.g. "xlsx" or ".xlsx", with no actual basename
    stop("`save` must be an ", paste0(options, collapse = " or "), " file", call. = FALSE)
  }
}
