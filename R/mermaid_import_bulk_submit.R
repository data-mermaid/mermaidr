#' Bulk submit valid records in Collecting
#'
#' Bulk submit validated records in Collecting for a given project. Only submits records that have successfully validated without ANY errors or warnings. To be used after \code{\link{mermaid_import_project_data}} and \code{\link{mermaid_import_bulk_validate}}.
#'
#' @inheritParams get_project_endpoint
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' \dontrun{
#' p <- mermaid_get_my_projects() %>%
#'   head(1)
#'
#' p %>%
#'   mermaid_import_bulk_validate()
#'
#' # 43 records being validated...
#' # ✖ 27 records produced errors in validation
#' # • 13 records produced warnings in validation
#' # ✔ 3 records successfully validated without warnings or errors
#'
#' p %>%
#'   mermaid_import_bulk_submit()
#' # 3 records being submitted...
# # ✔ 3 records successfully submitted
# # • 0 records were not successfully submitted
#' }
mermaid_import_bulk_submit <- function(project, token = mermaid_token()) {
  import_bulk_action(project, action = "submit", token = token)
}

summarise_submit_status <- function(df, status) {
  status <- df[["status"]] %>%
    as.character()
  n_status <- df[["n"]]

  plural <- plural(n_status)

  message <- dplyr::case_when(
    status == "ok" ~ glue::glue("{n_status} record{plural} successfully submitted"),
    status == "not_ok" ~ glue::glue("{n_status} record{plural} {plural_were(n_status)} not successfully submitted")
  )

  switch(status,
    "ok" = usethis::ui_done(message),
    "not_ok" = usethis::ui_todo(message)
  )
}
