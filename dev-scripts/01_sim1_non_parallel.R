library(pgxsim)
library(tidyverse)


fixed_df <- data_frame(type='discrete', mu=1, sd=.5, lb=0.001, ub=30, ndoses=10, nreps=3, sd_prop=0.3, sd_add=0.15)
varying_df <- crossing(n=c(50, 200), prop=c(0.05,0.2), beta=log10(c(2,5,10))) %>%
  dplyr::mutate(sim_group=row_number())
complete_df <- crossing(fixed_df, varying_df, sim_rep=c(1:3)) %>%
  dplyr::mutate(sim_unique_id=row_number())

sim_cl_data <- complete_df %>%
  dplyr::sample_n(10) %>%
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
  dplyr::mutate(lm_fit=purrr::map(data, ~lm(pIC50 ~ gene, .)),
                lm_res=purrr::map(lm_fit, broom::tidy)) %>%
  dplyr::select(-lm_fit, -data) %>%
  tidyr::unnest() %>%
  dplyr::filter(term=='gene') %>%
  dplyr::mutate(method='lm')


#then do nlme plus lm
nlme_res <- sim_dr_data_calc %>%
  dplyr::mutate(fit=purrr::map(data, nlme_fit),
                res=purrr::map(fit, nlme_extract)) %>%
  dplyr::select(-data,-fit) %>%
  tidyr::unnest()

nlme_lm_res <- nlme_res %>%
  inner_join(sim_cl_data, by=c('sim_unique_id', 'cell_id')) %>%
  dplyr::select(sim_unique_id, cell_id, nlme_pIC50, gene) %>%
  dplyr::group_by(sim_unique_id) %>%
  tidyr::nest() %>%
  dplyr::mutate(lm_fit=purrr::map(data, ~lm(nlme_pIC50 ~ gene, .)),
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
combo_res <- bind_rows(lm_res, nlme_lm_res, nlme_gene_res) %>%
  inner_join(complete_df, by='sim_unique_id') %>%
  dplyr::select(-type, -mu, -sd, -sd_prop, -sd_add)

ggplot(combo_res, aes(beta, estimate, colour=method)) +
  geom_point() +
  facet_grid(prop~n) +
  theme_bw()

ggplot(combo_res %>% dplyr::filter(method=='lm'), aes(beta, -log10(p.value), colour=method)) +
  geom_point() +
  facet_grid(prop~n) +
  theme_bw()




