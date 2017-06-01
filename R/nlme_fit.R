#' Mixed Effects Model Fit
#'
#' Fit a dose response curve using \pkg{nlme}
#'
#' @param df A data frame with column names resp, conc and cell_id.
#'
#' @return an object of class `nlme` representing the nonlinea mixed-effects model fit.
#' @export
#'
#' @examples
#' #
nlme_fit <- function(df) {
  df <- nlme::groupedData(resp~conc|cell_id,data=df)
  nlme::nlme(resp~1-conc/(exp(ic50)+conc),
       fixed = ic50~1, random = nlme::pdDiag(ic50 ~1),
       data = df, start = 1,method='ML',
       verbose = FALSE, control =nlme::nlmeControl(pnlsMaxIter=10,tolerance=1e0)
  )

}
