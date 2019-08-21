#' Get MERMAID API token
#'
#' @param client_id API client id
#' @param client_secret API client secret
#'
#' @export
get_mermaid_token <- function(client_id = mermaid_api_client("id"), client_secret = mermaid_api_client("secret")) {

  payload <- list(
    client_id = client_id,
    client_secret = client_secret,
    audience = Sys.getenv("MERMAID_API_AUDIENCE"),
    grant_type = Sys.getenv("MERMAID_API_GRANT_TYPE")
  )

  resp <- httr::POST(Sys.getenv("MERMAID_API_TOKEN_URL"), body = payload, encode = "form")
  token_content <- httr::content(resp)
  token_expires <- Sys.time() + lubridate::seconds(token_content[["expires_in"]])
  token_content[["token_expires"]] <- token_expires

  Sys.setenv(MERMAID_API_TOKEN = token_content[["access_token"]])

  token_content
}
