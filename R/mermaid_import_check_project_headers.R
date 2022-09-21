mermaid_import_get_template <- function(method) {
  res <- mermaid_GET(glue::glue("ingest_schema_csv/{method}"))

  if (length(method) == 1) {
    res[[1]]
  }
}

construct_header_endpoint <- function(project, method) {

}
