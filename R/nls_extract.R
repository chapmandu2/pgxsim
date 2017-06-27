#' Extract pIC50s From nls
#'
#' Extract the pIC50 values from an nls fit.
#'
#' @param fit output from \code{\link{nls_fit}}
#' @param lower_trunc number of log10 units below the minimum concentration in the dose response
#' curve to truncate the pIC50 to.  Default is 3, ie pIC50 below -6 not allowed if minimum concentration
#' is 0.001uM.
#' @param upper_trunc number of log10 units above the maximum concentration in the dose response
#' curve to truncate the pIC50 to.  Default is 3, ie pIC50 above 4 not allowed if maximum concentration
#' is 10uM.
#'
#' @return data frame of values
#' @export
#'
#' @examples
#' library(tidyverse)
#' data(drc_data_small)
#' cl_data_small <- dplyr::group_by(drc_data_small, cell_id) %>% tidyr::nest()
#' fit <- nls_fit(cl_data_small$data[[1]])
#' fit
#' nls_extract(fit)
nls_extract <- function(fit, lower_trunc=3, upper_trunc=3) {
  if(!inherits(fit, 'nls')) {return(broom::tidy(NULL))}

  pIC50_min <- log10(min(fit$model$conc)) - lower_trunc
  pIC50_max <- log10(max(fit$model$conc)) + upper_trunc

  broom::tidy(fit) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(nls_pIC50 = log10(exp(.data$estimate)),
                  nls_pIC50 = dplyr::case_when(.data$nls_pIC50 > pIC50_max ~ pIC50_max,
                                               .data$nls_pIC50 < pIC50_min ~ pIC50_min,
                                               TRUE ~ .data$nls_pIC50),
                  nls_std_err = log10(exp(.data$std.error))
    ) %>%
    dplyr::select(-.data$estimate, -.data$std.error)
}
