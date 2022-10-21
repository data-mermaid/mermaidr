#' Get template for MERMAID import
#'
#' Get a template of the fields for importing into MERMAID. Used along with \code{\link{mermaid_import_get_options}}, which contains details on which fields are required and their possible options, if relevant. Optionally, the template can be saved into an Excel file using the \code{save} parameter.
#'
#' @param method Method to get import template for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity", or "all" (to get templates for all methods).
#' @param save Excel file to save template to. Optional.
#'
#' @return
#' @export
#'
#' @examples
mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity", "all"), save) {
  check_project_data_inputs(method, data = "all") # Faking data input to just allow for checking options

  if (any(method == "all")) {
    method <- c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity")
  }

  endpoints <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoints, mermaid_GET)

  single_method <- length(method) == 1

  if (single_method) {
    res <- res[[1]][[1]]
  } else {
    res <- purrr::map(res, ~ .x[[1]])
    names(res) <- method
    res <- res[method]
  }

  if (!missing(save)) {
    # Check that file is xlsx or xls
    pos <- regexpr("\\.([[:alnum:]]+)$", save)
    filetype <- substring(save, pos + 1L)

    if (!filetype %in% c("xlsx", "xls")) {
      stop("`save` must be an xls or xlsx file", call. = FALSE)
    }

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # If only one method, temporarily put it back in a named list
    if (single_method) {
      temp_res <- list(res)
      names(temp_res) <- method
    } else {
      temp_res <- res
    }

    # Iterate through methods and save each to a sheet
    purrr::imap(
      temp_res,
      function(data, method) {
        openxlsx::addWorksheet(wb, method)

        # Set to full width for each column
        openxlsx::setColWidths(wb, method, 1:ncol(data), nchar(names(data)) + 2) # Buffer

        openxlsx::writeData(wb, method, data)
      }
    )

    # Write workbook
    openxlsx::saveWorkbook(wb, save, overwrite = TRUE)

    usethis::ui_done("Import template written to {save}")
  }

  res
}
