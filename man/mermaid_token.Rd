% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_auth.R
\name{mermaid_token}
\alias{mermaid_token}
\title{Produce MERMAID token}
\usage{
mermaid_token(verbose = FALSE)
}
\arguments{
\item{verbose}{logical; do you want informative messages?}
}
\value{
a \code{request} object (an S3 class provided by \code{httr})
}
\description{
If token is not already available, call \code{\link{mermaid_auth}} to either load
from cache or initiate OAuth2.0 flow. Return the token -- not "bare" but,
rather, prepared for inclusion in downstream requests.
}
