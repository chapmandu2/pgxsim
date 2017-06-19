library(pgxsim)
library(tidyverse)

#here we adopt a simplified approach to doing the simulation having functions to
#carry out the different methods
#also calculate p-values at coefficient and test levels

fixed_df <- data_frame(type='discrete', mu=0, lb=-4, ub=Inf, sd=.5, minconc=0.001, maxconc=30, ndoses=10, nreps=3, sd_prop=0.3, sd_add=0.15)
varying_df <- crossing(n=c(50, 200), prop=c(0.05,0.2), beta=log10(c(2,5,10))) %>%
  dplyr::mutate(sim_group=row_number())
complete_df <- crossing(fixed_df, varying_df, sim_rep=c(1:3)) %>%
  dplyr::mutate(sim_unique_id=row_number())

sim_cl_data <- complete_df %>%
#  dplyr::sample_n(10) %>%
  dplyr::mutate(cl_data=purrr::pmap(.l=list(n=n, type=type, prop=prop), .f=sim_cell_lines)) %>%
  tidyr::unnest() %>%
  dplyr::select(-cl_sd_prop, -cl_sd_add) %>%
  dplyr::mutate(pIC50=purrr::pmap_dbl(.l=list(mu=mu, sd=sd, beta=beta, g=gene, n=1, type=type), .f=sim_pIC50))

#this step is slow!
sim_dr_data <- sim_cl_data %>%
  dplyr::mutate(dr_data = purrr::pmap(.l=list(pIC50=pIC50, minconc=minconc, maxconc=maxconc, ndoses=ndoses, nreps=nreps, sd_prop=sd_prop, sd_add=sd_add),
                                      .f=sim_dose_response))

#double nest the data so that there is one data frame per simulation
input_df <- sim_dr_data %>%
  dplyr::select(sim_unique_id, cell_id, gene, pIC50, dr_data) %>%
  dplyr::group_by(sim_unique_id) %>%
  tidyr::nest()

#test out the method functinos on their own
lm_method(input_df$data[[1]])
nlme_lm_method(input_df$data[[1]])
nls_lm_method(input_df$data[[1]])
nlme_gene_method(input_df$data[[1]])

#apply methods to multiple simulations
lm_res <- input_df %>%
  dplyr::transmute(sim_unique_id, res=purrr::map(data, lm_method)) %>%
  tidyr::unnest()
lm_res

#apply multiple methods to multiple simulations
all_res <- input_df %>%
  dplyr::mutate(lm_res=purrr::map(data, lm_method),
                nlme_lm_res=purrr::map(data, nlme_lm_method),
                nls_lm_method=purrr::map(data, nls_lm_method),
                nlme_gene_method=purrr::map(data, nlme_gene_method)) %>%
  dplyr::select(-data)
all_res

all_res %>%
  tidyr::gather(func, val, -sim_unique_id) %>%
  tidyr::unnest() %>%
  dplyr::select(-func)
