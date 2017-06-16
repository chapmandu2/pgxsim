library(pgxsim)
library(tidyverse)
set.seed(10001)

#generate dataset used in parallelisation vignette
# takes 2-3 mins
#define the parameters that will remain fixed
fixed_df <- data_frame(type='discrete', mu=0, lb=-4, ub=Inf, sd=1,
                       minconc=0.001, maxconc=30, ndoses=10, nreps=3,
                       sd_prop=0.3, sd_add=0.15, n=50)

#define the parameters that will vary
varying_df <- crossing(prop=c(0.1,0.3), beta=log10(c(2,5,10))) %>%
  dplyr::mutate(sim_group=row_number())

parallel_sim_df <- crossing(fixed_df, varying_df, sim_rep=c(1:40)) %>%
  dplyr::mutate(sim_unique_id=row_number(),
                batch=sample(1:16, n(), replace = TRUE))
parallel_sim_df

library(BatchJobs)

setConfig(cluster.functions=makeClusterFunctionsMulticore(ncpus=8))
bjwork_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
bjfiles_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
dir.create(bjwork_dir)
dir.create(bjfiles_dir)
bj_resources <- list()

#create the registry
reg <- makeRegistry(id="BatchJobs",
                    packages=c('pgxsim', 'tidyverse'), #packages to include
                    work.dir = bjwork_dir,
                    file.dir = bjfiles_dir)

#map
batchMap(reg, fun=subset_apply, k=1:max(parallel_sim_df$batch),
         more.args=list(df=parallel_sim_df, my_fun=do_simulation_type2))
#submitJobs(reg)
submitJobs(reg, resources = bj_resources)
showStatus(reg)
done <- findDone(reg)
job_info <- getJobInfo(reg)

#gather results and bind into a dataframe
parallel_res <- reduceResultsList(reg, findDone(reg))
parallel_res_df <- bind_rows(parallel_res) %>%
  inner_join(parallel_sim_df, by='sim_unique_id')

#tidy up
removeRegistry(reg, ask='no')


#make example datasets
devtools::use_data(parallel_res_df,
                    overwrite=TRUE)
