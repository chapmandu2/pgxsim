#' NLME gene method
#'
#' Estimate the genetic parameter directly within the non-linear model
#'
#' @param df nested data frame with cell_id, gene, pIC50 and dose response data
#'
#' @return data frame of results
#' @export
#'
#' @examples
#' NULL
nlme_gene_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(cell_id, gene, pIC50, dr_data) %>%
    tidyr::unnest()

  m0 <- nlme_fit(cleaned_df)
  m1 <- nlme_gene_fit(cleaned_df)

  test_stat <- -2*m0$logLik+2*m1$logLik
  test_pval <- 1-stats::pchisq(test_stat,1)

  broom::tidy(m1, effects='fixed') %>%
    dplyr::filter(term=='b') %>%
    dplyr::transmute(term, beta_estimate=estimate,
                     beta_std_err=std.error, beta_pval=p.value) %>%
    dplyr::mutate(test_rsq = NA,
                  test_stat = test_stat,
                  test_pval = test_pval,
                  test_df = NA,
                  method='nlme_gene')

}
