#' Simulate Cell Lines
#'
#' Simulate cell lines with predefined genetic and drug response characteristics.
#'
#' @param n Number of cell lines to simulate
#' @param type Whether genetic covariate is a discrete (d) or continuous (c) variable
#' @param prop Proportion of cell lines with a discrete characteristic
#' @param prho value of correlation coefficient (0-1 for continuous, ignored for discrete)
#' @param mu mean of the truncated normal distribution
#' @param sd standard deviation of the truncated normal distribution
#' @param beta value of the genetic coefficient to recover - note that this is on a log scale
#' @param lb lower bound of the pIC50 distribution
#' @param ub upper bound of the pIC50 distribution
#' @param sd_prop Cell line specific proportional dose response variance
#' @param sd_add Cell line specific additive dose response variance
#'
#' @return A tibble containing cell line data
#' @export sim_cell_lines
#'
#' @examples
#' library(dplyr)
#' set.seed(10000)
#' sim_cell_lines(n=10, type='d', prop=0.2, mu=1, sd=0.1, beta=1)
#' sim_cell_lines(n=10, type='c', mu=1, sd=0.1, beta=1, prho=0.6)
sim_cell_lines <- function(n, type='d', prop=0.2, prho=0.9, mu=0, sd=0.1, beta=1,
                           lb=-4, ub=Inf, sd_prop=0, sd_add=0) {

  if(grepl(paste0('^',type),'discrete')) {

    gene <- rep(0,n)
    idx <- sample(seq_len(n), prop*n)
    gene[idx] <- 1
    pIC50 <- sim_pIC50(mu, sd, beta, g=gene, n=n, type='d', lb=lb, ub=ub)

  } else if (grepl(paste0('^',type),'continuous')) {

    sigma <- matrix(c(sd^2,prho*(sd*1),
                      prho*(sd*1),1^2), ncol=2)
    res <- mvtnorm::rmvnorm(n=n, mean=c(mu,0), sigma=sigma)
    gene <- res[,2]
    pIC50 <- res[,1]

  } else {

    stop('Type must be d (discrete) or c (continuous)')
  }

  dplyr::data_frame(cell_id=1:n,
                    gene=gene,
                    pIC50=pIC50,
                    cl_sd_prop=sd_prop,
                    cl_sd_add=sd_add)
}
