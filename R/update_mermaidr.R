#' Update `mermaidr`
#'
#' Update `mermaidr` if you do not have the latest version. Once the update is done, you must restart R.
update_mermaidr <- function() {
  update_mermaidr_internal(main = TRUE)
}

update_mermaidr_internal <- function(main = TRUE) {
  if (check_mermaidr_update(main)) {
    usethis::ui_info("Updating `mermaidr`...")
    remotes::install_github("data-mermaid/mermaidr",
      upgrade = "never",
      ref = ifelse(main, "HEAD", "dev"),
      quiet = TRUE
    )
    usethis::ui_done("`mermaidr` updated!")
    usethis::ui_todo("Please restart R to use the latest version.")
  } else {
    usethis::ui_done("You already have the latest version of `mermaidr`!")
  }
}

check_mermaidr_update <- function(main = TRUE) {
  repo <- "data-mermaid/mermaidr"
  package <- "mermaidr"
  ref <- ifelse(main, "HEAD", "dev")
  remote <- remotes::github_remote(repo, ref)
  local_sha <- remotes:::local_sha(package)
  remote_sha <- remotes:::remote_sha(remote, package)

  !identical(local_sha, remote_sha)
}

mermaidr_update_needed <- function() {
  if (check_mermaidr_update()) {
    usethis::ui_todo("You do not have the latest version of `mermaidr`. Please run `update_mermaidr()` to update.")
  }
}
