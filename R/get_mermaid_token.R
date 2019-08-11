#' Get MERMAID API token
#'
#' @param client_id API client id
#' @param client_secret API client secret
#'
#' @export
get_mermaid_token <- function(client_id = Sys.getenv("MERMAID_API_CLIENT_ID"), client_secret = Sys.getenv("MERMAID_API_CLIENT_SECRET")) {
  client <- mermaid_api_client()

  payload <- list(
    client_id = client[["MERMAID_API_CLIENT_ID"]],
    client_secret = client[["MERMAID_API_CLIENT_SECRET"]],
    audience = Sys.getenv("MERMAID_API_AUDIENCE"),
    grant_type = Sys.getenv("MERMAID_API_GRANT_TYPE")
  )

  resp <- httr::POST(Sys.getenv("MERMAID_API_TOKEN_URL"), body = payload, encode = "form")
  token_content <- httr::content(resp)
  token_expires <- as.character(Sys.time() + lubridate::seconds(token_content[["expires_in"]]))

  Sys.setenv(MERMAID_API_TOKEN = token_content[["access_token"]])
  Sys.setenv(MERMAID_API_TOKEN_EXPIRES = token_expires)

  invisible(token_content)
}
