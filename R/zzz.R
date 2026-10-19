.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mermaidr <- list(
    mermaidr.httr_oauth_cache = TRUE
  )
  toset <- !(names(op.mermaidr) %in% names(op))
  if (any(toset)) options(op.mermaidr[toset])

  # Point mermaidr at another API (e.g. https://dev-api.datamermaid.org). Read at load
  # time: top-level assignments in settings.R are fixed when the package is installed.
  api_url <- Sys.getenv("MERMAID_API_URL")
  if (nzchar(api_url)) {
    ns <- topenv()
    assign("base_url", api_url, envir = ns)
    assign("mermaid_audience", api_url, envir = ns)
  }

  if (curl::has_internet() & base_url == "https://api.datamermaid.org") { # Only when in prod
    mermaidr_update_needed()
  }

  invisible()
}
