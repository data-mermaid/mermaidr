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
mermaid_get_me <- function(token = mermaid_token(), field_report = TRUE) {
  res <- mermaid_GET("me", token = token, field_report = field_report)

  res[["me"]]
}
