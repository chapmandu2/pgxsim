#' Subset and Apply Function
#'
#' Subset a data frame and apply a function to that portion of it
#'
#' @param k batch id to select
#' @param df data frame to subset
#' @param my_fun function to apply
#'
#' @return
#' @export
#'
#' @examples
#' NULL
subset_apply <- function(k, df, my_fun) {
  df %>%
    dplyr::filter(batch==k) %>%
    my_fun()

}
