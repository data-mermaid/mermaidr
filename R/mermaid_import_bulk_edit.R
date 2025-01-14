method_to_methods_endpoint <- function(method) {
  methods_endpoint_names[[method]]
}

methods_endpoint_names <- list(
  fishbelt = "beltfishtransectmethods",
  benthiclit = "benthiclittransectmethods",
  benthicpit = "benthicpittransectmethods",
  benthicpqt = "benthicphotoquadrattransectmethods",
  bleaching = "bleachingquadratcollectionmethods",
  habitatcomplexity = "habitatcomplexitytransectmethods"
)

protocol_to_endpoint_names <- function(method) {
  protocol_methods_endpoint_names[[method]]
}

protocol_methods_endpoint_names <- list(
  beltfishes = "beltfishtransectmethods",
  benthiclits = "benthiclittransectmethods",
  benthicpits = "benthicpittransectmethods",
  benthicpqts = "benthicphotoquadrattransectmethods",
  bleachingqcs = "bleachingquadratcollectionmethods",
  habitatcomplexities = "habitatcomplexitytransectmethods"
)
