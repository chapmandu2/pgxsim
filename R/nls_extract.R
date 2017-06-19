#' Extract pIC50s From nls
#'
#' Extract the pIC50 values from an nls fit.
#'
#' @param fit output from \code{\link{nls_fit}}
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
nls_extract <- function(fit) {
  if(!inherits(fit, 'nls')) {return(broom::tidy(NULL))}
  broom::tidy(fit) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(nls_pIC50 = log10(exp(.data$estimate)),
                  nls_std_err = log10(exp(.data$std.error))
    ) %>%
    dplyr::select(-.data$estimate, -.data$std.error)
}
