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
#' NULL
nlme_lm_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(.data$cell_id, .data$gene, .data$pIC50, .data$dr_data)

  nlme_results <- cleaned_df %>%
    tidyr::unnest() %>%
    nlme_fit() %>%
    nlme_extract()

  combined_df <- cleaned_df %>%
    dplyr::select(-.data$dr_data) %>%
    dplyr::inner_join(nlme_results, by='cell_id')

  lm_method(combined_df, 'nlme_pIC50') %>%
    dplyr::mutate(method='nlme_lm')
}
