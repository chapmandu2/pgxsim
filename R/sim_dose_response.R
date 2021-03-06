#' Simulate Dose Response Data
#'
#' Simulates dose response data for a cell line given a pIC50, upper and lower bounds, number
#' of doses, number of replicates per dose and additive and proportional variance
#'
#' @param pIC50 The pIC50 for the cell line
#' @param minconc Lowest concentration the cell line is tested at
#' @param maxconc Highest concentration the cell line is tested at
#' @param ndoses Number of doses
#' @param nreps Numer of replicates
#' @param sd_prop Proportional standard deviation
#' @param sd_add Additive standard deviation
#'
#' @return A data frame of dose response values
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(10000)
#' #no error
#' ex1 <- sim_dose_response(1, 0.001, 30, 10, 3, 0, 0)
#' ex1
#' qplot(conc, resp, data=ex1) + scale_x_log10()
#'
#' #some proportional error
#' ex2 <- sim_dose_response(1, 0.001, 30, 10, 3, 0.1, 0)
#' ex2
#' qplot(conc, resp, data=ex2) + scale_x_log10()
#'
#' #some additive error
#' ex3 <- sim_dose_response(1, 0.001, 30, 10, 3, 0, 0.1)
#' ex3
#' qplot(conc, resp, data=ex3) + scale_x_log10()
#'
#' #both error types
#' ex4 <- sim_dose_response(1, 0.001, 30, 10, 3, 0, 0.1)
#' ex4
#' qplot(conc, resp, data=ex3) + scale_x_log10()
sim_dose_response <- function(pIC50=1, minconc=0.001, maxconc=30, ndoses=10, nreps=3, sd_prop=0.1, sd_add=0.1) {
  concs <- 10^(seq(log10(minconc), log10(maxconc), by=(log10(maxconc)-log10(minconc))/(ndoses-1)))
  tidyr::crossing(conc=concs, rep=1:nreps) %>%
    dplyr::mutate(resp_raw = 1*(1 - 1/(1+((10^(pIC50))/.data$conc))),
                  resp_sd_prop = stats::rnorm(ndoses*nreps, 0, sd_prop),
                  resp_sd_add = stats::rnorm(ndoses*nreps, 0, sd_add),
                  resp = .data$resp_raw * (1 + .data$resp_sd_prop) + .data$resp_sd_add) %>%
    dplyr::select(-.data$resp_sd_prop, -.data$resp_sd_add)

}
