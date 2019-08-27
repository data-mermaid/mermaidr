base_url <- "https://dev-api.datamermaid.org/"
ua <- httr::user_agent("https://github.com/data-mermaid/mermaidr")

check_limit <- function(limit) {
  if (length(limit) != 1) {
    stop("`limit` must be a length 1 positive integer vector.",
         call. = FALSE
    )
  } else if (!is.numeric(limit) ||
             !(limit %% 1 == 0) ||
             limit <= 0 ||
             limit == Inf) {
    stop("`limit` must be a positive integer.",
         call. = FALSE
    )
  } else {
    limit
  }
}
