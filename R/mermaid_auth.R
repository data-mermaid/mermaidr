# Very much "borrowed" from `googlesheets`: https://github.com/jennybc/googlesheets/blob/master/R/gs_auth.R

# environment to store credentials
.state <- new.env(parent = emptyenv())

#' Authorize \code{mermaidr}
#'
#' Authorize \code{mermaidr} to access the MERMAID API. You will be
#' directed to a web browser and asked to sign in to MERMAID Collect. By default, these user credentials are cached in a
#' file named \code{.httr-oauth} in the current working directory, from where
#' they can be automatically refreshed, as necessary.
#'
#' Most users, most of the time, do not need to call this function
#' explicitly -- it will be triggered by the first action that
#' requires authorization. Even when called, the default arguments will often
#' suffice. However, when necessary, this function allows the user to
#'
#' \itemize{
#'   \item force the creation of a new token
#'   \item retrieve current token as an object, for possible storage to an
#'   \code{.rds} file
#'   \item read the token from an object or from an \code{.rds} file
#'   \item prevent caching of credentials in \code{.httr-oauth}
#' }
#'
#' In a direct call to \code{mermaid_auth}, the user can dictate whether interactively-obtained
#' credentials will be cached in \code{.httr_oauth}. If unspecified, these
#' arguments are controlled via options, which, if undefined at the time
#' \code{mermaidr} is loaded, are defined like so:
#'
#' \describe{
#'   \item{cache}{Set to option \code{mermaidr.httr_oauth_cache}, which
#'   defaults to \code{TRUE}}
#' }
#'
#' To override these defaults in persistent way, predefine one or more of
#' them with lines like this in a \code{.Rprofile} file:
#' \preformatted{
#' options(mermaidr.httr_oauth_cache = FALSE)
#' }
#' See \code{\link[base]{Startup}} for possible locations for this file and the
#' implications thereof.
#'
#' @param token optional; an actual token object or the path to a valid token
#'   stored as an \code{.rds} file
#' @param new_user logical, defaults to \code{FALSE}. Set to \code{TRUE} if you
#'   want to wipe the slate clean and re-authenticate with the same or different account. This disables the \code{.httr-oauth} file in current
#'   working directory.
#' @param key the "Client ID" for the application
#' @param cache logical indicating if \code{mermaidr} should cache
#'   credentials in the default cache file \code{.httr-oauth}
#' @param verbose logical; do you want informative messages?
#'
#' @family auth functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' mermaid_auth()
#'
#' ## force a new token to be obtained
#' mermaid_auth(new_user = TRUE)
#'
#' ## store token in an object and then to file
#' ttt <- mermaid_auth()
#' saveRDS(ttt, "ttt.rds")
#'
#' ## load a pre-existing token
#' mermaid_auth(token = ttt) # from an object
#' mermaid_auth(token = "ttt.rds") # from .rds file
#' }
mermaid_auth <- function(token = NULL,
                         new_user = FALSE,
                         key = Sys.getenv("MERMAID_OAUTH_API_CLIENT_ID"),
                         cache = TRUE,
                         verbose = TRUE,
                         silent = FALSE) {
  if (new_user) {
    mermaid_deauth(clear_cache = TRUE, verbose = verbose)
  }

  if (is.null(token)) {
    if (!silent) {
      mermaid_endpoint <- httr::oauth_endpoint(
        authorize = "https://datamermaid.auth0.com/authorize",
        access = "https://datamermaid.auth0.com/oauth/token"
      )
    } else if (silent) {
      mermaid_endpoint <- httr::oauth_endpoint(
        authorize = "https://datamermaid.auth0.com/authorize",
        access = "https://datamermaid.auth0.com/oauth/token",
        prompt = "none"
      )
    }
    mermaid_app <- httr::oauth_app("mermaidr", key = key, secret = NULL)
    mermaid_token <-
      httr::oauth2.0_token(mermaid_endpoint, mermaid_app)
    stopifnot(is_legit_token(mermaid_token, verbose = TRUE))
    .state$token <- mermaid_token
    .state$token_expires <- Sys.time() + mermaid_token$credentials$expires_in
  } else if (inherits(token, "Token2.0")) {
    stopifnot(is_legit_token(token, verbose = TRUE))
    .state$token <- token
  } else if (inherits(token, "character")) {
    mermaid_token <- try(suppressWarnings(readRDS(token)), silent = TRUE)
    if (inherits(mermaid_token, "try-error")) {
      spf("Cannot read token from alleged .rds file:\n%s", token)
    } else if (!is_legit_token(mermaid_token, verbose = TRUE)) {
      spf("File does not contain a proper token:\n%s", token)
    }
    .state$token <- mermaid_token
  } else {
    spf(
      "Input provided via 'token' is neither a",
      "token,\nnor a path to an .rds file containing a token."
    )
  }

  invisible(.state$token)
}

#' Produce MERMAID token
#'
#' If token is not already available, call \code{\link{mermaid_auth}} to either load
#' from cache or initiate OAuth2.0 flow. Return the token -- not "bare" but,
#' rather, prepared for inclusion in downstream requests. Use
#' \code{access_token()} to reveal the actual access token, suitable for use
#' with \code{curl}.
#'
#' @return a \code{request} object (an S3 class provided by \code{httr})
#'
#' @keywords internal
mermaid_token <- function(verbose = FALSE, silent = FALSE) {
  if (!token_available(verbose = verbose)) mermaid_auth(verbose = verbose, silent = silent)
  httr::config(token = .state$token)
}

#' @rdname mermaid_token
include_token_if <- function(cond) if (cond) mermaid_token() else NULL
#' @rdname mermaid_token
omit_token_if <- function(cond) if (cond) NULL else mermaid_token()

#' Check token availability
#'
#' Check if a token is available in \code{mermaidr}' internal
#' \code{.state} environment.
#'
#' @return logical
#'
#' @keywords internal
token_available <- function(verbose = TRUE) {
  if (is.null(.state$token)) {
    if (verbose) {
      if (file.exists(".httr-oauth")) {
        message(
          "A .httr-oauth file exists in current working ",
          "directory.\nWhen/if needed, the credentials cached in ",
          ".httr-oauth will be used for this session.\nOr run mermaid_auth() ",
          "for explicit authentication and authorization."
        )
      } else {
        message(
          "No .httr-oauth file exists in current working directory.\n",
          "When/if needed, 'mermaidr' will initiate authentication ",
          "and authorization.\nOr run mermaid_auth() to trigger this ",
          "explicitly."
        )
      }
    }
    return(FALSE)
  }

  TRUE
}

#' Suspend authorization
#'
#' Suspend \code{mermaidr}' authorization to place requests to the
#' MERMAID APIs on behalf of the authenticated user.
#'
#' @param clear_cache logical indicating whether to disable the
#'   \code{.httr-oauth} file in working directory, if such exists, by renaming
#'   to \code{.httr-oauth-SUSPENDED}
#' @param verbose logical; do you want informative messages?
#' @export
#' @family auth functions
#' @examples
#' \dontrun{
#' mermaid_deauth()
#' }
mermaid_deauth <- function(clear_cache = TRUE, verbose = TRUE) {
  if (clear_cache && file.exists(".httr-oauth")) {
    if (verbose) {
      message("Disabling .httr-oauth by renaming to .httr-oauth-SUSPENDED")
    }
    file.rename(".httr-oauth", ".httr-oauth-SUSPENDED")
  }

  if (token_available(verbose = FALSE)) {
    if (verbose) {
      message("Removing MERMAID token stashed internally in 'mermaidr'.")
    }
    rm("token", envir = .state)
  } else {
    message("No token currently in force.")
  }

  invisible(NULL)
}

#' Check that token appears to be legitimate
#'
#' @keywords internal
is_legit_token <- function(x, verbose = FALSE) {
  if (!inherits(x, "Token2.0")) {
    if (verbose) message("Not a Token2.0 object.")
    return(FALSE)
  }

  if ("invalid_client" %in% unlist(x$credentials)) {
    # shouldn't happen if id and secret are good
    if (verbose) {
      message("Authorization error. Please check client_id.")
    }
    return(FALSE)
  }

  if ("invalid_request" %in% unlist(x$credentials)) {
    # in past, this could happen if user clicks "Cancel" or "Deny" instead of
    # "Accept" when OAuth2 flow kicks to browser ... but httr now catches this
    if (verbose) message("Authorization error. No access token obtained.")
    return(FALSE)
  }

  TRUE
}

## useful when debugging
access_token <- function() {
  if (!token_available(verbose = TRUE)) return(NULL)
  .state$token$credentials$access_token
}

spf <- function(...) stop(sprintf(...), call. = FALSE)
