#' Get or set MERMAID_API_CLIENT_ID and MERMAID_API_CLIENT_SECRET
#'
#' The API wrapper functions in this package all rely on a MERMAID API Client ID and Secret residing in the environment variables \code{MERMAID_API_CLIENT_ID} and \code{MERMAID_API_CLIENT_SECRET}. The
#' easiest way to accomplish this is to set them in the `\code{.Renviron}` file in your
#' home directory.
#'
#' @param force force setting a new MERMAID API Client ID and Secret for the current environment?
#'
#' @return atomic character vector containing the MERMAID API Client ID and Secret
#'
#' @export
mermaid_api_client <- function(force = FALSE) {
  client_id <- Sys.getenv("MERMAID_API_CLIENT_ID")
  client_secret <- Sys.getenv("MERMAID_API_CLIENT_SECRET")
  if (!identical(client_id, "") && !identical(client_secret, "") && !force) return(list(MERMAID_API_CLIENT_ID = client_id, MERMAID_API_CLIENT_SECRET = client_secret))

  if (!interactive()) {
    stop("Please set environment variables MERMAID_API_CLIENT_ID and MERMAID_API_CLIENT_SECRET to your MERMAID API Client ID and Secret",
      call. = FALSE
    )
  }

  message("Couldn't find environment variables MERMAID_API_CLIENT_ID and MERMAID_API_CLIENT_SECRET. See ?mermaid_api_client for more details.")
  message("Please enter your MERMAID API Client ID and press enter:")
  entered_client_id <- readline(": ")

  if (identical(entered_client_id, "")) {
    stop("MERMAID API Client ID entry failed", call. = FALSE)
  }

  message("Please enter your MERMAID API Client Secret and press enter:")
  entered_client_secret <- readline(": ")

  if (identical(entered_client_secret, "")) {
    stop("MERMAID API Client Secret entry failed", call. = FALSE)
  }

  message("Updating MERMAID_API_CLIENT_ID and MERMAID_API_CLIENT_SECRET.")
  Sys.setenv(MERMAID_API_CLIENT_ID = entered_client_id)
  Sys.setenv(MERMAID_API_CLIENT_SECRET. = entered_client_secret)

  list(
    MERMAID_API_CLIENT_ID = entered_client_id,
    MERMAID_API_CLIENT_SECRET = entered_client_secret
  )
}
