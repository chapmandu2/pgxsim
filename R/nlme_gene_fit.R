#' Mixed Effects Gene Model Fit
#'
#' Fit a dose response curve using \pkg{nlme} and including the genetic effect as a covariate
#'
#' @param df A data frame with column names resp, conc, gene and cell_id.
#'
#' @return an object of class `nlme` representing the nonlinear mixed-effects model fit.
#' @export
#'
#' @examples
#' data(drc_data_small)
#' fit <- nlme_gene_fit(drc_data_small)
#' fit
#' nlme_gene_extract(fit)
nlme_gene_fit <- function(df) {
  df <- nlme::groupedData(resp~conc|cell_id,data=df)
  nlme::nlme(resp~1-conc/(exp(ic50 + b*gene)+conc),
       fixed = ic50+b~1, random = nlme::pdDiag(ic50 ~1),
       data = df, start = c(1,1), method='ML',
       verbose = FALSE, control = nlme::nlmeControl(pnlsMaxIter=10,tolerance=1e0)
  )

}
