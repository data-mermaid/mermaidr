% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_get_managements.R
\name{mermaid_get_managements}
\alias{mermaid_get_managements}
\title{Get MERMAID managements}
\usage{
mermaid_get_managements(limit = NULL)
}
\arguments{
\item{limit}{Number of records to get. Use NULL (the default) to get all records.}
}
\description{
Get all MERMAID managements (not project specific). Returns metadata on managements, including ID and name, secondary name, rules, notes, year established, and when the management was created and last updated.
}
\examples{
\dontrun{
mermaid_get_managements(limit = 10)
}
}
