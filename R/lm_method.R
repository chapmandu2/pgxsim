#' LM method
#'
#' Simple linear regression and return output in standardised form
#'
#' @param df data frame including gene and pIC50 columns
#' @param pIC50_col name of the pIC50 column (default is pIC50)
#'
#' @return data frame of results
#' @export
#'
#' @examples
#' NULL
lm_method <- function(df, pIC50_col='pIC50') {

  cleaned_df <- dplyr::rename(df, est_pIC50=!!pIC50_col) %>%
      dplyr::select(cell_id, gene, est_pIC50)

  lm_fit <- stats::lm(est_pIC50 ~ gene, data=cleaned_df)

  out1 <- lm_fit %>%
    broom::tidy() %>%
    dplyr::filter(term=='gene') %>%
    dplyr::transmute(term, beta_estimate=estimate, beta_std_err=std.error, beta_pval=p.value)

  out2 <- lm_fit %>%
    broom::glance() %>%
    dplyr::transmute(test_rsq=r.squared, test_stat=statistic,
                     test_pval=p.value, test_df=df.residual)

  dplyr::bind_cols(out1,out2) %>%
    dplyr::mutate(method='lm_method')
}
