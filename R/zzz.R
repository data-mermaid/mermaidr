.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mermaidr <- list(
    mermaidr.httr_oauth_cache = TRUE
  )
  toset <- !(names(op.mermaidr) %in% names(op))
  if (any(toset)) options(op.mermaidr[toset])

  invisible()
}
