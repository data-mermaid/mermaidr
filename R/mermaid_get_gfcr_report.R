#' Get GFCR report for project(s)
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#' @param save Excel file to save GFCR report to - .xlsx or xls file. Optional.
#' @export
#'
#' @examples
#' \dontrun{
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   head(1) %>%
#'   mermaid_get_gfcr_report()
#' }
mermaid_get_gfcr_report <- function(project, save = NULL, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)

  gfcr_export_url <- httr::modify_url(base_url, path = "v1/reports/")

  if (length(project_id) == 1) {
    project_ids <- list(project_id)
  } else {
    project_ids <- project_id
  }

  gfcr_body <- list(
    "report_type" = "gfcr",
    "project_ids" = project_ids,
    "background" = "false"
  )

  temp_dir <- tempdir()
  temp_file <- tempfile() %>%
    basename()
  temp_zip <- glue::glue("{temp_dir}/{temp_file}.zip")
  report_dir <- glue::glue("{temp_dir}/{temp_file}")

  resp <- suppress_http_warning(
    httr::POST(gfcr_export_url, encode = "json", body = gfcr_body, ua, token, httr::write_disk(temp_zip))
  )

  check_errors(resp)

  # Check if zip -- if not, error
  if (resp$headers$`content-encoding` != "gzip") {
    stop("Error reading GFCR report. File was not downloaded as a ZIP, as is expected.", call. = FALSE)
  }

  utils::unzip(temp_zip, overwrite = TRUE, exdir = report_dir)

  # Check that the unzipped dir contains exactly one file .xlsx file and that it starts with "gfcr_"
  gfcr_report_file <- list.files(report_dir, full.names = TRUE)

  if (!(length(gfcr_report_file) == 1 & stringr::str_ends(gfcr_report_file, "xlsx") & stringr::str_starts(basename(gfcr_report_file), "gfcr"))) {
    stop("Error reading GFCR report. The download does not contain a single xlsx file, as expected.", call. = FALSE)
  }

  # If "save" is not NULL, copy the file to the location described
  if (!is.null(save)) {
    check_excel_file(save)

    file.copy(gfcr_report_file, save, overwrite = TRUE)

    usethis::ui_done("GFCR report written to {save}")
  }

  # Read all tabs in
  gfcr_report_sheets <- readxl::excel_sheets(gfcr_report_file)

  gfcr_report_sheets_res <- gfcr_report_sheets %>%
    purrr::map(\(x)
    readxl::read_xlsx(gfcr_report_file,
      sheet = x
    ) %>%
      dplyr::as_tibble())

  names(gfcr_report_sheets_res) <- gfcr_report_sheets

  gfcr_report_sheets_res
}
