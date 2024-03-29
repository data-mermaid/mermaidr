% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_get_project_data.R
\name{mermaid_get_project_data}
\alias{mermaid_get_project_data}
\title{Get MERMAID project data}
\usage{
mermaid_get_project_data(
  project = mermaid_get_default_project(),
  method = c("fishbelt", "benthiclit", "benthicpit", "benthicpqt", "bleaching",
    "habitatcomplexity", "all"),
  data = c("observations", "sampleunits", "sampleevents", "all"),
  limit = NULL,
  token = mermaid_token(),
  covariates = FALSE
)
}
\arguments{
\item{project}{A way to identify the project(s). Can be project IDs (passed as a character vector directly) or projects resulting from \code{\link{mermaid_get_my_projects}} or \code{\link{mermaid_search_my_projects}}. Defaults to the projects listed via \code{mermaid_get_default_project}, if available.}

\item{method}{Method to get data for. One of "fishbelt", "benthiclit", "benthicpit", "benthicpqt", bleaching", "habitatcomplexity", or "all" (to get data for all methods).}

\item{data}{Data to return. One of "observations", "sampleunits", "sampleevents", or "all" (to get all three kinds of data). See details for more.}

\item{limit}{Number of records to get. Use NULL (the default) to get all records.}

\item{token}{API token. Authenticate manually via \code{\link{mermaid_auth}}, or automatically when running any project- or user-specific functions (like this one).}

\item{covariates}{Whether to include covariates. Defaults to FALSE.}
}
\description{
Get Fish Belt, Benthic LIT, Benthic PIT, Bleaching, or Habitat Complexity data for your MERMAID projects. Data is available at the observation, sample unit, and sample event level. Optionally get covariates at the site level. See Details section for more. Requires authorization.
}
\details{
Fish Belt method data is available by setting \code{method} to "fishbelt". Fish Belt observations data contains individual observations recorded in MERMAID, while sample units contains total biomass in kg/ha per sample unit, by trophic group and by fish family. Sample events data contains \emph{mean} total biomass in kg/ha per sample event, and by trophic group and by fish family, as well as the  \emph{standard deviation} of the total biomass as well as per trophic group and fish family.

Benthic LIT method data is available by setting \code{method} to "benthiclit". Benthic LIT observations contain individual observations. Sample units data returns percent cover per sample unit, by benthic category. Sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.

Benthic PIT method data is available by setting \code{method} to "benthicpit". Similarly to Benthic LIT, Benthic PIT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category, and sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.

Benthic Photo Quadrat method data is available by setting \code{method} to "benthicpqt". Benthic PQT observations contain individual observations, sample units data returns percent cover per sample unit, by benthic category, and sample events contain \emph{mean} percent cover per sample event, by benthic category, and standard deviations of these values.

Bleaching method data is available by setting \code{method} to "bleaching". When Bleaching observations are requested, two types of observations are returned: Colonies Bleached and Percent Cover. Sample units data contains both Colonies Bleached data (number of coral genera and total number of colonies, and percent normal, pale, and bleached colonies) and Percent Cover data (Number of quadrats, and average percent cover for hard coral, soft coral, and macroalgae), all per sample unit. Sample events data contains \emph{mean} values of all the data in sample units, for both Colonies Bleached (average quadrat size, average number of coral genera and average total colonies, average percent normal, pale, and bleached colonies) and Percent Cover (average number of quadrats, and average of average hard coral, soft coral, and macroalgae cover). Mean percent cover values also come with standard deviations.

Habitat Complexity data is available by setting \code{method} to "habitatcomplexity". Observations contain individual observations, with the habitat complexity score at each interval. Sample units data contains the average habitat complexity score for the sample unit, and sample events data contains the average of those averages habitat complexity scores, along with standard deviations.

The included covariates are: geomorphic zonation and benthic habitat from the \href{https://allencoralatlas.org}{Allen Coral Atlas}; market gravity, water pollution (sediments and nitrogen), coastal population, industrial development (number of ports), tourism (reef value), and a cumulative pressure index from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12858}{\emph{A global map of human pressures on tropical coral reefs}} by Andrello et al., 2022; scores from \href{https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12587}{\emph{Risk-sensitive planning for conserving coral reefs under rapid climate change}} by Beyer et al., 2018.
}
\examples{
\dontrun{
projects <- mermaid_get_my_projects()
projects \%>\%
  mermaid_get_project_data(method = "fishbelt", data = "observations", limit = 10)

projects \%>\%
  mermaid_get_project_data(method = c("benthicpit", "fishbelt"), data = "sampleevents", limit = 10)

bleaching_obs <- projects \%>\%
  mermaid_get_project_data(method = "bleaching", data = "observations", limit = 10)
names(bleaching_obs)
# [1] "colonies_bleached" "percent_cover"
}
}
