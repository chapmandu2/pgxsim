#' Extract beta From nlme_gene fit
#'
#' Extract the beta coefficient from an nlme_gene fit.
#'
#' @param fit output from \code{\link{nlme_gene_fit}}
#'
#' @return data frame of values
#' @export
#'
#' @examples
#' data(drc_data_small)
#' fit <- nlme_gene_fit(drc_data_small)
#' fit
#' nlme_gene_extract(fit)
nlme_gene_extract <- function(fit) {
  broom::tidy(fit, effects='fixed') %>%
    dplyr::filter(term=='b') %>%
    dplyr::transmute(term, beta_estimate=log10(exp(estimate)),
                     beta_std_err=log10(exp(std.error)), beta_pval=p.value)
}
