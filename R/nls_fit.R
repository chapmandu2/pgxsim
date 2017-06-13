#' Non-linear Least Squares Fit
#'
#' Fit a dose response curve using the Levenberg-Marquardt Nonlinear Least-Squares squares
#' algorithm from the \pkg{minpack.lm} package.  Estimated pIC50 values can be extracted from
#' the fit using the \code{\link{nls_extract}} function.
#'
#' @param df A data frame with column names resp, conc and cell_id.
#'
#' @return an object of class `nls` representing the nonlinear least squares model fit.
#' @export
#'
#' @examples
#' data(drc_data_small)
#' fit <- nls_fit(drc_data_small)
#' fit
#' broom::tidy(fit)
nls_fit <- function(df) {
  minpack.lm::nlsLM(resp~1-conc/(exp(ic50)+conc),
                    data=df,
                    start = c(ic50=1))
}
