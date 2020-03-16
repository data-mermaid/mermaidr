#' Get endpoint from a specified MERMAID project
#'
#' @inheritParams mermaid_GET
#' @param project A way to identify the project. Can be a project ID (passed as a character vector directly) or a single project resulting from \code{\link{mermaid_get_endpoint}} or \code{\link{mermaid_search_projects}}. Defaults to the project listed via \code{get_default_project}, if available.
#'
#' @export
#' @examples
#' \dontrun{
#' test_project <- mermaid_search_projects("Sharla test")
#' mermaid_get_project_endpoint(test_project, "sites")
#' }
mermaid_get_project_endpoint <- function(project = mermaid_get_default_project(), endpoint = c("beltfishtransectmethods", "beltfishes", "benthiclittransectmethods", "benthicpittransectmethods", "benthicpits", "benthictransects", "collectrecords", "fishbelttransects", "habitatcomplexities", "obsbenthiclits", "obsbenthicpits", "obshabitatcomplexities", "obstransectbeltfishs", "managements", "observers", "profiles", "project_profiles", "sampleevents", "sites", "beltfishes/obstransectbeltfishes/", "beltfishes/sampleunits/", "beltfishes/sampleevents/"), limit = 50, url = base_url, token = mermaid_token()) {
  project_id <- as_id(project)
  check_project(project_id)
  endpoint <- match.arg(endpoint)

  full_endpoint <- paste0("projects/", project_id, "/", endpoint)
  res <- mermaid_GET(full_endpoint, limit = limit, url = url, token = token)

  if (endpoint %in% new_endpoints) {
    res
  } else {
    if (nrow(res) == 0) {
      cols <- mermaid_endpoint_columns[[ifelse(endpoint == "managements", "managements_project", endpoint)]]
      res <- tibble::as_tibble(matrix(nrow = 0, ncol = length(cols)), .name_repair = "minimal")
      names(res) <- cols
      res
    } else {
      res[, mermaid_endpoint_columns[[ifelse(endpoint == "managements", "managements_project", endpoint)]]]
    }
  }
}

check_project <- function(project) {
  if (project == "") {
    stop("Please supply a project to get data from, either via the `project` argument or by using `mermaid_set_default_project()`.", call. = FALSE)
  }
}

new_endpoints <- c("beltfishes/obstransectbeltfishes/", "beltfishes/sampleunits/", "beltfishes/sampleevents/")