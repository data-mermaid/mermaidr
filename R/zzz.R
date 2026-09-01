.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mermaidr <- list(
    mermaidr.httr_oauth_cache = TRUE
  )
  toset <- !(names(op.mermaidr) %in% names(op))
  if (any(toset)) options(op.mermaidr[toset])

  if (curl::has_internet() &
      !file.exists("DESCRIPTION") # TODO, just for development -- remove to merge
      ) {
    mermaidr_update_needed()
  }

  invisible()
}
