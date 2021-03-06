% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulationMetrics.R
\name{simulationMetrics}
\alias{simulationMetrics}
\title{Compute simulation metrics}
\usage{
simulationMetrics(simulation, thin = 1)
}
\arguments{
\item{simulation}{df.  result from simulator function}

\item{thin}{int. thinning results by thin timesteps}
}
\value{
A data frame with Dg and WSSscore
}
\description{
Compute simulation metrics
}
\examples{
simulation <- simulatorAll(grid = 10)
metrics <- simulationMetrics(simulation)

}
