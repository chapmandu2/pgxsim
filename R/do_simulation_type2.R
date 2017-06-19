#' Do Type 2 Simulation
#'
#' In this simulation data is simulated from scratch for each iteration, ie no data is shared
#' between simulations.  Test level statistics are calculated.
#'
#' @param df data frame containing columns named by parameter
#' @param output specify what to return: `results` for a data frame of results (default),
#' `data` to simulate data only or 'both' for a list containing both results and data
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data(sim_ex1_df)
#' do_simulation_type2(sim_ex1_df) #just return results
#' do_simulation_type2(sim_ex1_df, output='data') #just simulate dose response data
#' do_simulation_type2(sim_ex1_df, output='both') #return results and simulated data
#' }
do_simulation_type2 <- function(df, output='results') {

  if(!output %in% c('results', 'data', 'both')) {
    stop('output should be either results, data or both')
  }

  #simulate cell lines new method
  sim_cl_data <- df %>%
    dplyr::mutate(cl_data=purrr::pmap(.l=list(n=.data$n, type=.data$type, prop=.data$prop,
                                              mu=.data$mu, sd=.data$sd, beta=.data$beta,
                                              lb=.data$lb, ub=.data$ub),
                                      .f=sim_cell_lines)) %>%
    tidyr::unnest() %>%
    dplyr::select(-.data$cl_sd_prop, -.data$cl_sd_add)

  #TEST - JUST LM
  #input_df <- sim_cl_data %>%
  #   dplyr::select(sim_unique_id, cell_id, gene, pIC50) %>%
  #   dplyr::group_by(sim_unique_id) %>%
  #   tidyr::nest()

  #res_df <- input_df %>%
  #  dplyr::mutate(lm_res=purrr::map(data, lm_method)) %>%
  #  dplyr::select(-data) %>%
  #  tidyr::unnest()

  #return(res_df)
#END TEST

  #simulate dose response data
  sim_dr_data <- sim_cl_data %>%
    dplyr::mutate(dr_data = purrr::pmap(.l=list(pIC50=.data$pIC50, minconc=.data$minconc,
                                                maxconc=.data$maxconc, ndoses=.data$ndoses,
                                                nreps=.data$nreps, sd_prop=.data$sd_prop,
                                                sd_add=.data$sd_add),
                                        .f=sim_dose_response))

  #prepare data - one row per simulation
  input_df <- sim_dr_data %>%
    dplyr::select(.data$sim_unique_id, .data$cell_id, .data$gene, .data$pIC50, .data$dr_data) %>%
    dplyr::group_by(.data$sim_unique_id) %>%
    tidyr::nest()

  if(output == 'data') {return(input_df)}

  #apply different methods and gather the results
  res_df <- input_df %>%
    dplyr::mutate(lm_res=purrr::map(.data$data, lm_method),
                  nlme_lm_res=purrr::map(.data$data, nlme_lm_method),
                  nls_lm_method=purrr::map(.data$data, nls_lm_method),
                  nlme_gene_method=purrr::map(.data$data, nlme_gene_method)) %>%
    dplyr::select(-.data$data) %>%
    tidyr::gather('func', 'val', -.data$sim_unique_id) %>%
    tidyr::unnest() %>%
    dplyr::select(-.data$func)

  if(output == 'results') {
    return(res_df)
  } else {
    return(list(data=input_df, results=res_df))
  }
}
