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
    dplyr::mutate(cell_id=as.numeric(level),
           nlme_pIC50 = estimate #+ nlme::fixef(fit)['ic50']
    ) %>%
    dplyr::select( -group, -level, -estimate) %>%
    dplyr::arrange(cell_id)
}
