#' Get MERMAID project data
#'
#' @param method Method to get data for. One of "beltfishes", "benthicpits", or "all" (to get data for both methods).
#' @param data Data to return. One of "observations", "sampleunits", "sampleevents", or all (to get all three kinds of data). See details for more.
#' @inheritParams get_project_endpoint
#'
#' @export
#'
#' @examples
#' projects <- mermaid_get_my_projects()
#' projects %>%
#'   mermaid_get_project_data(method = "beltfishes", data = "sampleevents", limit = 10)
mermaid_get_project_data <- function(project, method = c("beltfishes", "benthicpit"), data = c("observations", "sampleunits", "sampleevents", "all"), limit = NULL, url = base_url, token = mermaid_token(), ...) {
  method <- match.arg(method, several.ok = FALSE)
  data <- match.arg(data, several.ok = FALSE)

  endpoint <- construct_endpoint(method, data)

  get_project_endpoint(project, endpoint, limit, url, token)
}

# TODO: expand to "all"

construct_endpoint <- function(method, data) {
  if (data == "observations") {
    data_endpoint <- switch(method,
                            beltfishes = "obstransectbeltfishes",
                            benthicpits = "obstransectbenthicpits")
  } else {
    data_endpoint <- data
  }

  paste0(method, "/", data_endpoint)
}
