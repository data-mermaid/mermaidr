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
mermaid_api_client <- function(value = c("id", "secret")) {
  value_string <- switch(value,
                         id = "ID",
                         secret = "Secret")
  environment_variable <- paste0("MERMAID_API_CLIENT_", toupper(value))
  client_value <- Sys.getenv(environment_variable)

  if(!identical(client_value, "")){
    return(client_value)
  }

  if(!interactive()){
    stop(paste("Please set environment variable", environment_variable, "to your MERMAID API Client ", value_string),
        call. = FALSE)
  }

  message(paste0("Couldn't find environment variables MERMAID_API_CLIENT_", toupper(value), ". See ?mermaid_api_client", value, " for more details."))
  message(paste("Please enter your MERMAID API Client", value_string, "and press enter:"))
  entered_value <- readline(": ")
}
