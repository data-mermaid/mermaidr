#' Get MERMAID reference
#'
#' Find the names and information of the fish and benthic attributes you can choose in MERMAID.
#'
#' @param reference MERMAID reference. One of fishfamilies, fishgenera, fishspecies, benthicattributes
#' @inheritParams mermaid_GET
#'
#' @export
#'
#' @examples
#' mermaid_get_reference("benthicattributes")
#' mermaid_get_reference(c("fishfamilies", "fishgenera"))
mermaid_get_reference <- function(reference = c("fishfamilies", "fishgenera", "fishspecies", "benthicattributes"), limit = NULL, url = base_url) {

  reference <- match.arg(reference, several.ok = TRUE)

  get_endpoint(reference, limit = limit, url = base_url)

}
