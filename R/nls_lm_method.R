#' NLS + LM method
#'
#' Estimate pIC50 for each cell line using non-linear least squares then do linear
#' regression
#'
#' @param df nested data frame with cell_id, gene, pIC50 and dose response data
#'
#' @return data frame of results
#' @export
#'
#' @examples
#' NULL
nls_lm_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(cell_id, gene, pIC50, dr_data)

  nls_results <- cleaned_df %>%
    dplyr::mutate(fit=purrr::map(dr_data, nls_fit),
                  res=purrr::map(fit, broom::tidy)) %>%
    dplyr::select(-dr_data,-fit) %>%
    tidyr::unnest()

  lm_method(nls_results, 'estimate') %>%
    dplyr::mutate(method='nls_lm')

}
