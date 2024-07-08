#' Generate a mermaid token
#'
#' Constructor function for objects of class [Mermaid2.0].
#'
#' @inheritParams httr::oauth2.0_token
#' @param refresh Whether the token is being refreshed. Defaults to FALSE.
#'
#' @noRd
mermaid2.0_token <- function(endpoint, app, scope = NULL, user_params = NULL,
                             type = NULL,
                             use_oob = getOption("httr_oob_default"),
                             oob_value = NULL,
                             as_header = TRUE,
                             use_basic_auth = FALSE,
                             cache = getOption("httr_oauth_cache"),
                             config_init = list(),
                             client_credentials = FALSE,
                             credentials = NULL,
                             query_authorize_extra = list(),
                             refresh = FALSE,
                             shiny = FALSE) {
  params <- list(
    scope = scope,
    user_params = user_params,
    type = type,
    use_oob = use_oob,
    oob_value = oob_value,
    as_header = as_header,
    use_basic_auth = use_basic_auth,
    config_init = config_init,
    client_credentials = client_credentials,
    query_authorize_extra = query_authorize_extra
  )

  Mermaid2.0$new(
    app = app,
    endpoint = endpoint,
    params = params,
    credentials = credentials,
    cache_path = if (is.null(credentials)) cache else FALSE,
    refresh = refresh,
    shiny = shiny
  )
}

renew_mermaid2.0 <- function(credentials, shiny = FALSE) {
  browser()
  mermaid_endpoint <- httr::oauth_endpoint(
    authorize = mermaid_authorize_url,
    access = mermaid_access_url
  )
  mermaid_app <- httr::oauth_app("mermaidr", key = mermaid_key, secret = NULL)
  renewed_token <- mermaid2.0_token(mermaid_endpoint, mermaid_app, refresh = !shiny, query_authorize_extra = list(audience = mermaid_audience))
  credentials$access_token <- renewed_token$credentials$access_token
  credentials
}

Mermaid2.0 <- R6::R6Class("Mermaid2.0", inherit = httr::Token2.0, list(
  initialize = function(app, endpoint, params = list(), credentials = NULL,
                        private_key = NULL,
                        cache_path = getOption("httr_oauth_cache"),
                        refresh = FALSE, shiny = FALSE) {
    self$app <- app
    self$endpoint <- endpoint
    self$params <- params
    self$cache_path <- httr:::use_cache(cache_path)
    self$private_key <- private_key

    if (!is.null(credentials)) {
      # Use credentials created elsewhere - usually for tests
      self$credentials <- credentials
      return(self)
    }

    # Are credentials cache already?
    if (self$load_from_cache() && !refresh) {
      self
    } else {
      self$init_credentials()
      self$cache()
    }
  },
  can_refresh = function() {
    TRUE
  },
  refresh = function() {
    cred <- renew_mermaid2.0(
      self$credentials,
      shiny = shiny
    )
    if (is.null(cred)) {
      remove_cached_token(self)
    } else {
      self$credentials <- cred
      self$cache()
    }
    self
  }
))
