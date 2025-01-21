#' Bulk validate records in Collecting
#'
#' Bulk validates records in Collecting for a given project, and returns information on how many records produced errors, produced warnings, or were successfully validated without errors or warnings. To be used after \code{\link{mermaid_import_project_data}}.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mermaid_get_my_projects() %>%
#'   head(1) %>%
#'   mermaid_import_bulk_validate()
#'
#' # 43 records being validated...
#' # ✖ 27 records produced errors in validation
#' # • 13 records produced warnings in validation
#' # ✔ 3 records successfully validated without warnings or errors
#' }
mermaid_import_bulk_validate <- function(project, token = mermaid_token()) {
  import_bulk_action(project, action = "validate", token = token)
}
