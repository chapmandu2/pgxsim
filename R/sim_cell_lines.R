#' Simulate Cell Lines
#'
#' Simulate a cell lines with predefined genetic characteristics
#'
#' @param n Number of cell lines to simulate
#' @param type Whether genetic covariate is a discrete (d) or continuous (c) variable
#' @param prop Proportion of cell lines with a discrete characteristic
#' @param sd_prop Cell line specific proportional dose response variance
#' @param sd_add Cell line specific additive dose response variance
#'
#' @return A tibble containing cell line data
#' @export sim_cell_lines
#'
#' @examples
#' library(dplyr)
#' set.seed(10000)
#' sim_cell_lines(n=10, type='d', prop=0.2)
#' sim_cell_lines(n=10, type='c')
sim_cell_lines <- function(n, type='d', prop=0.2, sd_prop=0, sd_add=0) {

  if(grepl(paste0('^',type),'discrete')) {
    gene <- as.numeric(stats::runif(n)<prop)
  } else if (grepl(paste0('^',type),'continuous')) {
    gene <- stats::rnorm(n)
  } else {
    stop('Type must be d (discrete) or c (continuous)')
  }

  dplyr::data_frame(cell_id=1:n,
                    gene=gene,
                    cl_sd_prop=sd_prop,
                    cl_sd_add=sd_add)
}
