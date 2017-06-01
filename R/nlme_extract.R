#' Extract pIC50s From nlme
#'
#' Extract the pIC50 values from an nlme fit.
#'
#' @param fit
#'
#' @return data frame of values
#' @export
#'
#' @examples
#' #
nlme_extract <- function(fit) {
  broom::tidy(fit, par_type='varying') %>%
    tbl_df() %>%
    mutate(cell_id=as.numeric(level),
           nlme_pIC50 = estimate + nlme::fixef(fit)['ic50']
    ) %>%
    dplyr::select( -group, -level, -estimate) %>%
    arrange(cell_id)
}
