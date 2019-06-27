base_url <- "https://dev-api.datamermaid.org/"
ua <- httr::user_agent("https://github.com/data-mermaid/mermaidr")

# check json
check_json <- function(resp) {
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
}

# check for errors
check_errors <- function(resp) {
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Mermaid API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
}
