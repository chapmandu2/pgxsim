#' Do Type 2 Simulation
#'
#' In this simulation data is simulated from scratch for each iteration, ie no data is shared
#' between simulations.  Test level statistics are calculated.
#'
#' @param df data frame containing columns named by parameter
#'
#' @return data frame
#' @export
#'
#' @examples
#' NULL
do_simulation_type2 <- function(df) {

  #simulate cell lines new method
  sim_cl_data <- df %>%
    dplyr::mutate(cl_data=purrr::pmap(.l=list(n=n, type=type, prop=prop, mu=mu, sd=sd, beta=beta),
                                      .f=sim_cell_lines)) %>%
    tidyr::unnest() %>%
    dplyr::select(-cl_sd_prop, -cl_sd_add)

  #simulate dose response data
  sim_dr_data <- sim_cl_data %>%
    dplyr::mutate(dr_data = purrr::pmap(.l=list(pIC50, lb, ub, ndoses, nreps, sd_prop, sd_add),
                                        .f=sim_dose_response))

  #prepare data - one row per simulation
  input_df <- sim_dr_data %>%
    dplyr::select(sim_unique_id, cell_id, gene, pIC50, dr_data) %>%
    dplyr::group_by(sim_unique_id) %>%
    tidyr::nest()

  #for testing
  #input_df <- sim_cl_data %>%
  #   dplyr::select(sim_unique_id, cell_id, gene, pIC50) %>%
  #   dplyr::group_by(sim_unique_id) %>%
  #   tidyr::nest()

  #apply different methods and gather the results
  res_df <- input_df %>%
    dplyr::mutate(lm_res=purrr::map(data, lm_method),
                  nlme_lm_res=purrr::map(data, nlme_lm_method),
                  nls_lm_method=purrr::map(data, nls_lm_method),
                  nlme_gene_method=purrr::map(data, nlme_gene_method)) %>%
    dplyr::select(-data) %>%
    tidyr::gather(func, val, -sim_unique_id) %>%
    tidyr::unnest() %>%
    dplyr::select(-func)

  return(res_df)
}
