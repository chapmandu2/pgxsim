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
  if(!inherits(fit, 'nlme')) {return(broom::tidy(NULL))}
  broom::tidy(fit, effects='fixed') %>%
    dplyr::filter(.data$term=='b') %>%
    dplyr::transmute(.data$term, beta_estimate=log10(exp(.data$estimate)),
                     beta_std_err=log10(exp(.data$std.error)), beta_pval=.data$p.value)
}
