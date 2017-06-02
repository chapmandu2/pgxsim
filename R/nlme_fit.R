#' Mixed Effects Model Fit
#'
#' Fit a dose response curve using \pkg{nlme}.  Estimated pIC50 values can be extracted from
#' the fit using the \code{\link{nlme_extract}} function.
#'
#' @param df A data frame with column names resp, conc and cell_id.
#'
#' @return an object of class `nlme` representing the nonlinea mixed-effects model fit.
#' @export
#'
#' @examples
#' data(drc_data_small)
#' fit <- nlme_fit(drc_data_small)
#' fit
#' nlme_extract(fit)
nlme_fit <- function(df) {
  df <- nlme::groupedData(resp~conc|cell_id,data=df)
  nlme::nlme(resp~1-conc/(exp(ic50)+conc),
       fixed = ic50~1, random = nlme::pdDiag(ic50 ~1),
       data = df, start = 1,method='ML',
       verbose = FALSE, control =nlme::nlmeControl(pnlsMaxIter=10,tolerance=1e0)
  )

}
