#' Get your MERMAID Profile
#'
#' Get your MERMAID profile. Returns ID, first/last name, and email address. Requires authorization.
#'
#' @inheritParams mermaid_GET
#'
#' @return A tibble of MERMAID profile information, including first and last name, full name, and email address.
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_me()
#' }
mermaid_get_me <- function(token = mermaid_token()) {
  res <- mermaid_GET("me", token = token)

  res[["me"]]
}
