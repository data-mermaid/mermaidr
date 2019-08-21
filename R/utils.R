base_url <- "https://dev-api.datamermaid.org/"
ua <- httr::user_agent("https://github.com/data-mermaid/mermaidr")

# check json
check_json <- function(resp) {
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
}

# check for erro2rs
check_errors <- function(response, parsed) {
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Mermaid API request failed [%s]\n%s",
        httr::status_code(response),
        parsed$detail
      ),
      call. = FALSE
    )
  }
}
