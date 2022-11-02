#' Get template and field options for MERMAID import
#'
#' @param method Method to get import template and field options for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", or "habitatcomplexity".
#' @param save Excel file to save template and field options to - .xlsx or xls file. Optional.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' template_and_options <- mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_import_get_template_and_options("fishbelt", "fishbelt_template.xlsx")
#' # ✔ Import template and field options written to fishbelt_template.xlsx
#'
#' names(template_and_options)
#' # [1]  "Template"                   "Site *"
#' # [3]  "Management *"               "Sample date: Year *"
#' # [5]  "Sample date: Month *"       "Sample date: Day *"
#' # [7]  "Sample time"                "Depth *"
#' # [9]  "Transect number *"          "Transect label"
#' # [11] "Transect length surveyed *" "Width *"
#' # [13] "Fish size bin *"            "Reef slope"
#' # [15] "Visibility"                 "Current"
#' # [17] "Relative depth"             "Tide"
#' # [19] "Sample unit notes"          "Observer emails *"
#' # [21] "Fish name *"                "Size *"
#' # [23] "Count *"
#'
#' template_and_options[["Template"]]
#'
#' # A tibble: 0 × 22
#' # … with 22 variables: Site * <chr>, Management * <chr>, Sample date: Year * <chr>,
#' #   Sample date: Month * <chr>, Sample date: Day * <chr>, Sample time <chr>, Depth * <chr>,
#' #   Transect number * <chr>, Transect label <chr>, Transect length surveyed * <chr>,
#' #   Width * <chr>, Fish size bin * <chr>, Reef slope <chr>, Visibility <chr>, Current <chr>,
#' #   Relative depth <chr>, Tide <chr>, Sample unit notes <chr>, Observer emails * <chr>,
#' #   Fish name * <chr>, Size * <chr>, Count * <chr>
#'
#' template_and_options[["Reef slope"]][["required"]]
#' # [1] FALSE
#'
#' template_and_options[["Reef slope"]][["choices"]]
#' # A tibble: 4 × 1
#' # value
#' # <chr>
#' # 1 crest
#' # 2 flat
#' # 3 slope
#' # 4 wall
#' }
mermaid_import_get_template_and_options <- function(project, method, save, token = mermaid_token()) {
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

  template <- mermaid_import_get_template(method)

  options <- mermaid_import_get_options(project, method, token = mermaid_token())

  if (!missing(save)) {
    # Check that file is xlsx or xls
    check_excel_file(save)

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Write sheet with template
    openxlsx::addWorksheet(wb, "Template")

    # Set to full width for each column
    openxlsx::setColWidths(wb, "Template", 1:ncol(template), nchar(names(template)) + 2) # Buffer

    openxlsx::writeData(wb, "Template", template)

    # Write a metadata sheet with description/required
    openxlsx::addWorksheet(wb, "Metadata")
    openxlsx::writeData(wb, "Metadata", startCol = 1, "Column Name")
    openxlsx::writeData(wb, "Metadata", startCol = 2, "Required")
    openxlsx::writeData(wb, "Metadata", startCol = 3, "Description")

    metadata_row <- 2

    # Iterate through through fields and save each to a sheet
    purrr::imap(options, function(field_data, field_name) {
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
    openxlsx::setColWidths(wb, "Metadata", 1, max(nchar(names(options))) + 2) # Buffer

    # Write workbook
    openxlsx::saveWorkbook(wb, save, overwrite = TRUE)

    usethis::ui_done("Import template and field options written to {save}")
  }

  # Return template and options
  append(list(Template = template), options)
}

check_import_inputs <- function(method, data) {
  if (!all(method %in% c("fishbelt", "benthicpit", "benthicpqt", "benthiclit", "habitatcomplexity", "bleaching")) | length(method) > 1) {
    stop('`method` must be one of: "fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching", "habitatcomplexity"', call. = FALSE)
  }
}

mermaid_import_get_template <- function(method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity")) {
  endpoint <- glue::glue("ingest_schema_csv/{method}")

  res <- purrr::map(endpoint, mermaid_GET)

  res[[1]][[1]]
}

mermaid_import_get_options <- function(project, method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleachingqc", "habitatcomplexity"), token = mermaid_token()) {
  endpoint <- glue::glue("collectrecords/ingest_schema/{method}")

  res <- purrr::map(endpoint, ~ get_project_endpoint(project, .x, limit = NULL, token))

  res <- purrr::map(res, clean_import_options)

  res[[1]]
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

clean_excel_sheet_name <- function(name) {
  name %>%
    stringr::str_remove_all(":") %>%
    stringr::str_remove_all("\\*") %>%
    stringr::str_trim() %>%
    stringr::str_sub(1, 31)
}
