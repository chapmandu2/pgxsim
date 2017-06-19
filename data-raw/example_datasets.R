library(pgxsim)
library(tidyverse)
set.seed(10001)
#simulate cell lines
cell_lines_small <- sim_cell_lines(n=10, type='d', prop=0.2)[,1:2]
cell_lines_large <- sim_cell_lines(n=100, type='d', prop=0.2)[,1:2]

#simulate pIC50 values
pIC50_data_small <- dplyr::mutate(cell_lines_small,
                                  pIC50=sim_pIC50(0,1,beta=1, gene, n=nrow(cell_lines_small), type='d'))
pIC50_data_large <- dplyr::mutate(cell_lines_large,
                                  pIC50=sim_pIC50(0,1,beta=1, gene, n=nrow(cell_lines_large), type='d'))

#simulate dose response data
drc_data_small <- dplyr::mutate(pIC50_data_small,
                                data=purrr::map(pIC50, ~sim_dose_response(pIC50=., minconc=0.001, maxconc=30,
                                                                          ndoses=10, nreps=3, sd_prop=0.1, sd_add=0.1)))
drc_data_small <- tidyr::unnest(drc_data_small)

drc_data_large <- dplyr::mutate(pIC50_data_large,
                                data=purrr::map(pIC50, ~sim_dose_response(pIC50=., minconc=0.001, maxconc=30,
                                                                          ndoses=10, nreps=3, sd_prop=0.1, sd_add=0.1)))
drc_data_large <- tidyr::unnest(drc_data_large)

#data frame to define simulation
sim_ex1_df <- data_frame(type='discrete', mu=0, lb=-4, ub=Inf, sd=1,
                         minconc=0.001, maxconc=30, ndoses=10, nreps=3,
                         sd_prop=0.3, sd_add=0.15, n=50, beta=1, prop=0.5,
                         sim_unique_id=1:3)

#data frame of data for sim_ex1_df
set.seed(10001)
sim_ex1_data <- do_simulation_type2(sim_ex1_df, output='data')

#make example datasets
devtools::use_data(cell_lines_small,
                    cell_lines_large,
                    pIC50_data_small,
                    pIC50_data_large,
                    drc_data_small,
                    drc_data_large,
                    sim_ex1_df,
                    sim_ex1_data,
                    overwrite=TRUE)
