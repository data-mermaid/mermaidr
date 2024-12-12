mermaid_export_project_gfcr_report <- function(project, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)

  gfcr_export_url <- httr::modify_url(base_url, path = "v1/reports/")
  gfcr_body <- list(
    "report_type" = "gfcr",
    "project_ids" = project_id,
    "background" = "false"
  )

  temp_dir <- tempdir()
  temp_file <- tempfile() %>%
    basename()
  temp_zip <- glue::glue("{temp_dir}/{temp_file}.zip")
  report_dir <- glue::glue("{temp_dir}/{temp_file}")

  resp <- httr::POST(gfcr_export_url, encode = "multipart", body = gfcr_body, ua, token, httr::write_disk(temp_zip))

  check_errors(resp)

  # Check if zip -- if not, error
  if (resp$headers$`content-type` != "application/zip") {
    stop("Error reading GFCR report. File was not downloaded as a ZIP, as is expected.", call. = FALSE)
  }

  utils::unzip(temp_zip, overwrite = TRUE, exdir = report_dir)

  # Check that the unzipped dir contains exactly one file .xlsx file and that it starts with "gfcr_"
  gfcr_report_file <- list.files(report_dir, full.names = TRUE)

  if (!(length(gfcr_report_file) == 1 & stringr::str_ends(gfcr_report_file, "xlsx") & stringr::str_starts(basename(gfcr_report_file), "gfcr"))) {
    stop("Error reading GFCR report. The download does not contain a single xlsx file, as expected.", call. = FALSE)
  }

  # Read all tabs in
  gfcr_report_sheets <- openxlsx::getSheetNames(gfcr_report_file)

  gfcr_report_sheets_res <- gfcr_report_sheets %>%
    purrr::map(\(x)
    openxlsx::read.xlsx(gfcr_report_file,
      sheet = x,
      check.names = FALSE,
      sep.names = " "
    ) %>%
      dplyr::as_tibble())

  names(gfcr_report_sheets_res) <- gfcr_report_sheets

  gfcr_report_sheets_res
}
