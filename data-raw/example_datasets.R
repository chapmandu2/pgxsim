library(pgxsim)
set.seed(10001)
#simulate cell lines
cell_lines_small <- sim_cell_lines(n=10, type='d', prop=0.2)[,1:2]
cell_lines_large <- sim_cell_lines(n=100, type='d', prop=0.2)[,1:2]

#simulate pIC50 values
pIC50_data_small <- dplyr::mutate(cell_lines_small,
                                  pIC50=sim_pIC50(1,1,beta=1, gene, n=nrow(cell_lines_small), type='d'))
pIC50_data_large <- dplyr::mutate(cell_lines_large,
                                  pIC50=sim_pIC50(1,1,beta=1, gene, n=nrow(cell_lines_large), type='d'))

#simulate dose response data
drc_data_small <- dplyr::mutate(pIC50_data_small,
                                data=purrr::map(pIC50, ~sim_dose_response(pIC50=., lb=0.001, ub=30,
                                                                          ndoses=10, nreps=3, sd_prop=0.1, sd_add=0.1)))
drc_data_small <- tidyr::unnest(drc_data_small)

drc_data_large <- dplyr::mutate(pIC50_data_large,
                                data=purrr::map(pIC50, ~sim_dose_response(pIC50=., lb=0.001, ub=30,
                                                                          ndoses=10, nreps=3, sd_prop=0.1, sd_add=0.1)))
drc_data_large <- tidyr::unnest(drc_data_large)

#make example datasets
devtools::use_data(cell_lines_small,
                    cell_lines_large,
                    pIC50_data_small,
                    pIC50_data_large,
                    drc_data_small,
                    drc_data_large,
                    overwrite=TRUE)
