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
#' data("sim_ex1_data")
#' dat <- sim_ex1_data$data[[1]]
#' dat
#' nlme_gene_method(dat)
#'
#' library(tidyverse)
#' purrr::map(sim_ex1_data$data, nlme_gene_method) %>% dplyr::bind_rows()
nlme_gene_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(.data$cell_id, .data$gene, .data$pIC50, .data$dr_data) %>%
    tidyr::unnest()

  m0 <- purrr::possibly(nlme_fit, NULL)(cleaned_df)
  m1 <- purrr::possibly(nlme_gene_fit, NULL)(cleaned_df)

  if(!inherits(m0, 'nlme') | !inherits(m1, 'nlme')) {
    return(broom::tidy(NULL))
  }

  test_stat <- -2*m0$logLik+2*m1$logLik
  test_pval <- 1-stats::pchisq(test_stat,1)

  nlme_gene_extract(m1) %>%
    dplyr::mutate(test_rsq = NA,
                  test_stat = test_stat,
                  test_pval = test_pval,
                  test_df = NA,
                  method='nlme_gene')

}
