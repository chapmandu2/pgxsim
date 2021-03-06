% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlme_gene_extract.R
\name{nlme_gene_extract}
\alias{nlme_gene_extract}
\title{Extract beta From nlme_gene fit}
\usage{
nlme_gene_extract(fit)
}
\arguments{
\item{fit}{output from \code{\link{nlme_gene_fit}}}
}
\value{
data frame of values
}
\description{
Extract the beta coefficient from an nlme_gene fit.
}
\examples{
data(drc_data_small)
fit <- nlme_gene_fit(drc_data_small)
fit
nlme_gene_extract(fit)
}
