#' NLME + LM method
#'
#' Estimate pIC50 values for each cell line using non-linear mixed model
#' then do linear regression
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
#' nlme_lm_method(dat)
#'
#' library(tidyverse)
#' purrr::map(sim_ex1_data$data, nlme_lm_method) %>% dplyr::bind_rows()
nlme_lm_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(.data$cell_id, .data$gene, .data$pIC50, .data$dr_data)

  nlme_results <- cleaned_df %>%
    tidyr::unnest() %>%
    purrr::possibly(nlme_fit, NULL)() %>%
    nlme_extract()

  if(nrow(nlme_results)==0) {return(broom::tidy(NULL))}

  combined_df <- cleaned_df %>%
    dplyr::select(-.data$dr_data) %>%
    dplyr::inner_join(nlme_results, by='cell_id')

  lm_method(combined_df, 'nlme_pIC50') %>%
    dplyr::mutate(method='nlme_lm')
}
