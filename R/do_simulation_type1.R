#' Do Type 1 Simulation
#'
#' In this simulation data is simulated from scratch for each iteration, ie no data is shared
#' between simulations
#'
#' @param df data frame containing columns named by parameter
#'
#' @return data frame
#' @export
#'
#' @examples
#' NULL
do_simulation_type1 <- function(df) {
  sim_cl_data <- df %>%
    dplyr::mutate(cl_data=purrr::pmap(.l=list(n, type, prop), .f=sim_cell_lines)) %>%
    tidyr::unnest() %>%
    dplyr::select(-cl_sd_prop, -cl_sd_add) %>%
    dplyr::mutate(pIC50=purrr::pmap_dbl(.l=list(mu, sd, beta, g=gene, n=1, type), .f=sim_pIC50))

  #this step is slow!
  sim_dr_data <- sim_cl_data %>%
    dplyr::mutate(dr_data = purrr::pmap(.l=list(pIC50, lb, ub, ndoses, nreps, sd_prop, sd_add),
                                        .f=sim_dose_response))

  sim_dr_data_calc <- sim_dr_data %>%
    dplyr::select(sim_unique_id, cell_id, gene, pIC50, dr_data) %>%
    tidyr::unnest() %>%
    dplyr::group_by(sim_unique_id) %>%
    tidyr::nest()

  #first do lm with raw pIC50s
  lm_res <- sim_cl_data %>%
    dplyr::select(sim_unique_id, cell_id, gene, pIC50) %>%
    dplyr::group_by(sim_unique_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(lm_fit=purrr::map(data, ~stats::lm(pIC50 ~ gene, .)),
                  lm_res=purrr::map(lm_fit, broom::tidy)) %>%
    dplyr::select(-lm_fit, -data) %>%
    tidyr::unnest() %>%
    dplyr::filter(term=='gene') %>%
    dplyr::mutate(method='lm')

  #then do nls plus lm
  nls_res <- sim_dr_data %>%
    dplyr::select(sim_unique_id, cell_id, gene, pIC50, dr_data)  %>%
    dplyr::mutate(fit=purrr::map(dr_data, nls_fit),
                  res=purrr::map(fit, broom::tidy)) %>%
    dplyr::select(-dr_data,-fit) %>%
    tidyr::unnest()

  nls_lm_res <- nls_res %>%
    dplyr::select(sim_unique_id, cell_id, estimate, gene) %>%
    dplyr::group_by(sim_unique_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(lm_fit=purrr::map(data, ~stats::lm(estimate ~ gene, .)),
                  lm_res=purrr::map(lm_fit, broom::tidy)) %>%
    dplyr::select(-lm_fit, -data) %>%
    tidyr::unnest() %>%
    dplyr::filter(term=='gene') %>%
    dplyr::mutate(method='nls_lm')


  #then do nlme plus lm
  nlme_res <- sim_dr_data_calc %>%
    dplyr::mutate(fit=purrr::map(data, nlme_fit),
                  res=purrr::map(fit, nlme_extract)) %>%
    dplyr::select(-data,-fit) %>%
    tidyr::unnest()

  nlme_lm_res <- nlme_res %>%
    dplyr::inner_join(sim_cl_data, by=c('sim_unique_id', 'cell_id')) %>%
    dplyr::select(sim_unique_id, cell_id, nlme_pIC50, gene) %>%
    dplyr::group_by(sim_unique_id) %>%
    tidyr::nest() %>%
    dplyr::mutate(lm_fit=purrr::map(data, ~stats::lm(nlme_pIC50 ~ gene, .)),
                  lm_res=purrr::map(lm_fit, broom::tidy)) %>%
    dplyr::select(-lm_fit, -data) %>%
    tidyr::unnest() %>%
    dplyr::filter(term=='gene') %>%
    dplyr::mutate(method='nlme_lm')

  #finally do nlme
  nlme_gene_res <- sim_dr_data_calc %>%
    dplyr::mutate(fit=purrr::map(data, nlme_gene_fit)) %>%
    dplyr::mutate(res=map(fit, broom::tidy, effects='fixed')) %>%
    dplyr::select(-data, -fit) %>%
    tidyr::unnest() %>%
    dplyr::filter(grepl('b', term)) %>%
    dplyr::mutate(method='nlme_gene')

  #combine all results
  combo_res <- bind_rows(lm_res, nlme_lm_res, nls_lm_res, nlme_gene_res) %>%
    dplyr::inner_join(df, by='sim_unique_id') %>%
    dplyr::select(-type, -mu, -sd, -sd_prop, -sd_add)

  return(combo_res)
}
