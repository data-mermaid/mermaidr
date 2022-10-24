#' Get template for MERMAID import
#'
#' Get a template of the fields for importing into MERMAID. Used along with \code{\link{mermaid_import_get_options}}, which contains details on which fields are required and their possible options, if relevant. Optionally, the template can be saved into an Excel file using the \code{save} parameter.
#'
#' @param method Method to get import template for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", or "habitatcomplexity".
#' @param save Excel file to save template to. Optional.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_import_get_template("fishbelt")
#' }
mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity"), save) {
  check_import_inputs(method)

  endpoints <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoints, mermaid_GET)

  res <- res[[1]][[1]]

  if (!missing(save)) {
    # Check that file is xlsx or xls
    pos <- regexpr("\\.([[:alnum:]]+)$", save)
    filetype <- substring(save, pos + 1L)

    if (!filetype %in% c("xlsx", "xls")) {
      stop("`save` must be an xls or xlsx file", call. = FALSE)
    }

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Save to a sheet
    openxlsx::addWorksheet(wb, method)

    # Set to full width for each column
    openxlsx::setColWidths(wb, method, 1:ncol(res), nchar(names(res)) + 2) # Buffer

    openxlsx::writeData(wb, method, res)

    # Write workbook
    browser()
    openxlsx::saveWorkbook(wb, save, overwrite = TRUE)

    usethis::ui_done("Import template written to {save}")
  }

  res
}

check_import_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching")) | length(method) > 1) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"', call. = FALSE)
  }
}
