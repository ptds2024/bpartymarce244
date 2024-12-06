% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volume_summary.R
\name{volume_summary}
\alias{volume_summary}
\title{Volume Summary}
\usage{
volume_summary(sim_results_df)
}
\arguments{
\item{sim_results_df}{A data frame with `volume` and `surface` columns.}
}
\value{
A named list with 99th percentile, mean, and standard deviation of volume and surface.
}
\description{
Calculates and returns the volume summary statistics based on simulation results.
}
\examples{
sim_results_df <- data.frame(volume = runif(100, 300, 1000), surface = runif(100, 50, 150))
volume_summary(sim_results_df)
}
\author{
MarcelaChoque
}
