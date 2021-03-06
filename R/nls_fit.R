#' Non-linear Least Squares Fit
#'
#' Fit a dose response curve using the Levenberg-Marquardt Nonlinear Least-Squares squares
#' algorithm from the \pkg{minpack.lm} package.  Estimated pIC50 values can be extracted from
#' the fit using the \code{\link{nls_extract}} function.
#'
#' NOTE: sometimes the fit will fail, in which case null is returned
#'
#' @param df A data frame with column names resp, conc and cell_id.
#'
#' @return an object of class `nls` representing the nonlinear least squares model fit.
#' @export
#'
#' @examples
#' data(drc_data_small)
#' cl1_data <- dplyr::filter(drc_data_small, cell_id==1)
#' fit <- nls_fit(cl1_data)
#' fit
#' broom::tidy(fit)  #on log-normal scale
#' nls_extract(fit)  #convenience function converts scales automatically
nls_fit <- function(df) {
  res <- try(
    minpack.lm::nlsLM(resp~1-conc/(exp(ic50)+conc),
                      data=df,
                      start = c(ic50=1),
                      model=TRUE),
    silent=TRUE)

  if(inherits(res, 'try-error')) {
    return(NULL)
  } else {
    return(res)
  }
}
