#' Extract pIC50s From nlme
#'
#' Extract the pIC50 values from an nlme fit.
#'
#' @param fit output from \code{\link{nlme_fit}}
#'
#' @return data frame of values
#' @export
#'
#' @examples
#' data(drc_data_small)
#' fit <- nlme_fit(drc_data_small)
#' fit
#' nlme_extract(fit)
nlme_extract <- function(fit) {
  broom::tidy(fit, par_type='varying') %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(cell_id=as.numeric(.data$level),
           nlme_pIC50 = log10(exp(.data$estimate))
    ) %>%
    dplyr::select( -.data$group, -.data$level, -.data$estimate) %>%
    dplyr::arrange(.data$cell_id)
}
