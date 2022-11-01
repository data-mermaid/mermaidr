#' Get field options for MERMAID import
#'
#' Check the options available for importing a given method into MERMAID to see what the options are for each field. Returns a list of each field for a given method, whether it's required (in \code{required}), any available description or help with the field (in \code{help_text}), and a list of available choices, if relevant (in \code{choices}). Optionally, the fields and options can be saved into an Excel file using the \code{save} parameter. See also \code{\link{mermaid_import_check_options}} for checking that your data matches the allowed values.
#'
#' @param method Method to get import fields and options for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", or "habitatcomplexity".
#' @param save Excel file to save field options to - .xlsx or xls file. Optional.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' options <- mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_import_get_options("fishbelt", "fishbelt.xlsx")
#'
#' names(options)
#' # [1]  "Site *"                     "Management *"
#' # [3]  "Sample date: Year *"        "Sample date: Month *"
#' # [5]  "Sample date: Day *"         "Sample time"
#' # [7]  "Depth *"                    "Transect number *"
#' # [9]  "Transect label"             "Transect length surveyed *"
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
mermaid_import_get_options <- function(project, method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"), save, token = mermaid_token()) {
  project <- as_id(project)
  check_project(project)

  # Check only one project
  if (length(project) > 1) {
    stop("Please supply only one project", call. = FALSE)
  }

  check_import_inputs(method)

  if (method == "bleaching") {
    method <- "bleachingqc"
  }

  endpoint <- glue::glue("collectrecords/ingest_schema/{method}")

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit = NULL, token))

  res <- purrr::map(res, clean_import_options)

  res <- res[[1]]

  if (!missing(save)) {
    # Check that file is xlsx or xls
    check_excel_file(save)

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Write a metadata sheet with description/required
    openxlsx::addWorksheet(wb, "Metadata")
    openxlsx::writeData(wb, "Metadata", startCol = 1, "Column Name")
    openxlsx::writeData(wb, "Metadata", startCol = 2, "Required")
    openxlsx::writeData(wb, "Metadata", startCol = 3, "Description")

    metadata_row <- 2

    # Iterate through through fields and save each to a sheet
    purrr::imap(res, function(field_data, field_name) {
      # Write name, required, and help text to metadata sheet
      openxlsx::writeData(wb, "Metadata",
        startCol = 1, startRow = metadata_row,
        field_name
      )
      openxlsx::writeData(wb, "Metadata",
        startCol = 2, startRow = metadata_row,
        ifelse(field_data[["required"]], "Yes", "No")
      )
      openxlsx::writeData(wb, "Metadata",
        startCol = 3, startRow = metadata_row,
        field_data[["help_text"]]
      )

      metadata_row <<- metadata_row + 1

      # Only write sheet for column if choices is not NULL

      # Add "choices" if not NULL
      if (!is.null(field_data[["choices"]])) {

        # Need to remove : and * from field names, limit to 31 characters
        field_name <- field_name %>%
          clean_excel_sheet_name()

        # Name sheets by the field name
        openxlsx::addWorksheet(wb, field_name)

        openxlsx::writeData(wb, field_name, field_data[["choices"]][["value"]])
      }
    })

    # Make metadata sheet column name column full width
    openxlsx::setColWidths(wb, "Metadata", 1, max(nchar(names(res))) + 2) # Buffer

    # Write workbook
    openxlsx::saveWorkbook(wb, save, overwrite = TRUE)

    usethis::ui_done("Import field options written to {save}")
  }

  res
}

clean_excel_sheet_name <- function(name) {
  name %>%
    stringr::str_remove_all(":") %>%
    stringr::str_remove_all("\\*") %>%
    stringr::str_trim() %>%
    stringr::str_sub(1, 31)
}

clean_import_options <- function(data) {
  data <- data[[1]]

  # Name each list element with "label"
  labels <- data %>% purrr::map_chr("label")
  names(data) <- labels

  # Remove "label" field and "name"
  data <- data %>%
    purrr::map(function(x) {
      x[["label"]] <- NULL
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
