#' NLS + LM method
#'
#' Estimate pIC50 for each cell line using non-linear least squares then do linear
#' regression.  Two results are generated, one where the estimated pIC50 is truncated to the max/min of the
#' dose response curve (nls_fit_t0), and one where it is truncated to 3 units above/below the max/min (nls_lm).
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
#' nls_lm_method(dat)
#'
#' library(tidyverse)
#' purrr::map(sim_ex1_data$data, nls_lm_method) %>% dplyr::bind_rows()
nls_lm_method <- function(df) {

  cleaned_df <- df %>%
    dplyr::select(.data$cell_id, .data$gene, .data$pIC50, .data$dr_data)

  nls_fits <- cleaned_df %>%
    dplyr::mutate(fit=purrr::map(.data$dr_data, nls_fit))

  nls_results1 <- nls_fits %>%
    dplyr::mutate(res=purrr::map(.data$fit, nls_extract, lower_trunc=3, upper_trunc=3)) %>%
    dplyr::select(-.data$dr_data,-.data$fit) %>%
    tidyr::unnest() %>%
    lm_method(pIC50_col = 'nls_pIC50') %>%
    dplyr::mutate(method='nls_lm')

  nls_results2 <- nls_fits %>%
    dplyr::mutate(res=purrr::map(.data$fit, nls_extract, lower_trunc=0, upper_trunc=0)) %>%
    dplyr::select(-.data$dr_data,-.data$fit) %>%
    tidyr::unnest() %>%
    lm_method(pIC50_col = 'nls_pIC50') %>%
    dplyr::mutate(method='nls_lm_t0')

  dplyr::bind_rows(nls_results1, nls_results2)

}
