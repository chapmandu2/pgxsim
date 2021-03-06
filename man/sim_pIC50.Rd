% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_pIC50.R
\name{sim_pIC50}
\alias{sim_pIC50}
\title{Simulate pIC50 value}
\usage{
sim_pIC50(mu, sd, beta, g, n = 1, type = "d", lb = -4, ub = Inf)
}
\arguments{
\item{mu}{mean of the truncated normal distribution}

\item{sd}{standard deviation of the truncated normal distribution}

\item{beta}{value of the genetic coefficient to recover - note that this is on a log scale}

\item{g}{value of the gene (0/1 for discrete or Z-value for continuous)}

\item{n}{number of data points to generate (default is 1)}

\item{type}{Whether genetic covariate is a discrete (d) or continuous (c) variable}

\item{lb}{lower bound of the pIC50 distribution}

\item{ub}{upper bound of the pIC50 distribution}
}
\value{
numeric
}
\description{
Uses the truncated normal distribution from the msm package to simulate a pIC50
value for a cell line.  Incorporates genetic covariate information.
}
\examples{
library(dplyr)
set.seed(10000)
sim_pIC50(0, 1, 0, 0, type='d')
sim_pIC50(0, 1, 0, 0, n=10, type='d')
sim_cell_lines(n=10, type='d', prop=0.2) \%>\%
 dplyr::mutate(pIC50=sim_pIC50(0,1,beta=1,gene, n=n(), type='d'))
}
