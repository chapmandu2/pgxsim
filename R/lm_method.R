#' LM method
#'
#' Simple linear regression and return output in standardised form.  Requires a data frame
#' containing a column named gene and a column containing pIC50 data whose name can be
#' specified using the `pIC50_col` parameter
#'
#' @param df data frame including gene and pIC50 columns
#' @param pIC50_col name of the pIC50 column (default is pIC50)
#'
#' @return data frame of results
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data("pIC50_data_small")
#' lm_method(pIC50_data_small)
#'
#' data("sim_ex1_data")
#' dat <- sim_ex1_data$data[[1]]
#' dat
#' lm_method(dat)
lm_method <- function(df, pIC50_col='pIC50') {

  pIC50_col <- rlang::enquo(pIC50_col)

  cleaned_df <- dplyr::rename(df, est_pIC50=!!pIC50_col) %>%
      dplyr::select(.data$cell_id, .data$gene, .data$est_pIC50)

  lm_fit <- stats::lm(est_pIC50 ~ gene, data=cleaned_df)

  out1 <- lm_fit %>%
    broom::tidy() %>%
    dplyr::filter(.data$term=='gene') %>%
    dplyr::transmute(.data$term, beta_estimate=.data$estimate,
                     beta_std_err=.data$std.error, beta_pval=.data$p.value)

  out2 <- lm_fit %>%
    broom::glance() %>%
    dplyr::transmute(test_rsq=.data$r.squared, test_stat=.data$statistic,
                     test_pval=.data$p.value, test_df=.data$df.residual)

  dplyr::bind_cols(out1,out2) %>%
    dplyr::mutate(method='lm_method')
}
