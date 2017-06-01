#' Simulate pIC50 value
#'
#' Uses the truncated normal distribution from the msm package to simulate a pIC50
#' value for a cell line.  Incorporates genetic covariate information.
#'
#' @param mu mean of the truncated normal distribution
#' @param sd standard deviation of the truncated normal distribution
#' @param beta value of the genetic coefficient to recover - note that this is on a log scale
#' @param g value of the gene (0/1 for discrete or Z-value for continuous)
#' @param n number of data points to generate (default is 1)
#' @param type Whether genetic covariate is a discrete (d) or continuous (c) variable
#' @param lb lower bound of the distribution
#' @param ub upper bound of the distribution
#'
#' @return numeric
#' @export sim_pIC50
#' @importFrom magrittr "%>%"
#'
#' @examples
#' library(dplyr)
#' sim_pIC50(1, 1, 0, 0, type='d')
#' sim_pIC50(1, 1, 0, 0, n=10, type='d')
#' sim_cell_lines(n=10, type='d', prop=0.2) %>%
#'  dplyr::mutate(pIC50=sim_pIC50(1,1,beta=1,gene, n=n(), type='d'))
sim_pIC50 <- function(mu, sd, beta, g, n=1, type='d', lb=-1.5, ub=Inf) {

  if(grepl(paste0('^',type),'discrete')) {

    #simulate from truncated normal distribution
        msm::rtnorm(n,mu*(1 + g*beta),sd,lb,ub)

    } else if (grepl(paste0('^',type),'continuous')) {

      warning('Continuous not implemented yet')
      return(rep(0, n))

    } else {
    stop('Type must be d (discrete) or c (continuous)')
  }


}


