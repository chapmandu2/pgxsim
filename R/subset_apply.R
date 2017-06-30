#' Subset and Apply Function
#'
#' Subset a data frame and apply a function to that portion of it defined by a column named batch
#'
#' @param k batch id to select
#' @param df data frame to subset
#' @param my_fun function to apply
#' @param seed seed value to add to batch id, makes simulations reproducible.  Default is NULL, don't set seed.
#'
#' @return output of function my_fun
#' @export
#'
#' @examples
#' test_df <- data.frame(batch=1:10, x=rnorm(10))
#' test_df
#' test_fun <- function(df) {df$y <- df$x * rnorm(1); return(df)}
#' test_fun(test_df)
#' test_fun(test_df[1:3,])
#' subset_apply(1, test_df, test_fun)
#' subset_apply(1, test_df, test_fun)
#' subset_apply(1, test_df, test_fun, seed=1001)
#' subset_apply(1, test_df, test_fun, seed=1001)
subset_apply <- function(k, df, my_fun, seed=NULL) {
  if(!is.null(seed)) {
    set.seed(seed+k)
  }
  df %>%
    dplyr::filter(.data$batch==k) %>%
    my_fun()

}
