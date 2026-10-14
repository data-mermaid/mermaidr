#' Update \code{mermaidr}
#'
#' Update \code{mermaidr} if you do not have the latest version. Once the update is done, you must restart R.
#' @export
update_mermaidr <- function() {
  update_mermaidr_internal(main = TRUE)
}

update_mermaidr_internal <- function(main = TRUE) {
  if (check_mermaidr_update(main, user_prompted = TRUE)) {
    cli::cli_alert_info("Updating `mermaidr`...")
    remotes::install_github("data-mermaid/mermaidr",
      upgrade = "never",
      ref = ifelse(main, "HEAD", "dev"),
      quiet = TRUE
    )
    cli::cli_alert_success("`mermaidr` updated!")
    cli::cli_alert_warning("Please restart R to use the latest version.")
  } else {
    cli::cli_alert_success("You already have the latest version of `mermaidr`!")
  }
}

check_mermaidr_update <- function(main = TRUE, user_prompted = FALSE) {
  repo <- "data-mermaid/mermaidr"
  package <- "mermaidr"
  ref <- ifelse(main, "HEAD", "dev")

  update_needed <- try(
    {
      remote <- remotes::github_remote(repo, ref)
      local_sha <- remotes:::local_sha(package)
      remote_sha <- remotes::remote_sha(remote, package)

      !identical(local_sha, remote_sha)
    },
    silent = TRUE
  )

  if (inherits(update_needed, "try-error")) {
    if (user_prompted) { # Even if it errors, return TRUE because the user wants to update -- so try again to do that
      return(TRUE)
    }
    return(FALSE)
  } else {
    update_needed
  }
}

mermaidr_update_needed <- function() {
  if (check_mermaidr_update()) {
    packageStartupMessage("You do not have the latest version of `mermaidr`. Please run `update_mermaidr()` to update.")
  }
}
