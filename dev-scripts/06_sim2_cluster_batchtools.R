library(pgxsim)
library(tidyverse)

#work up a simple simulation example for scaling up to cluster
#use batchtools rather than BatchJobs
#to make harder add remove n=50 from fixed_df and add n=c(50,200,800) to varying_df

#define the parameters that will remain fixed
fixed_df <- data_frame(type='discrete', mu=0, lb=-4, ub=Inf, sd=1,
                       minconc=0.001, maxconc=30, ndoses=10, nreps=3,
                       sd_prop=0.3, sd_add=0.15, n=50)

#define the parameters that will vary
varying_df <- crossing(prop=c(0.1,0.3), beta=log10(c(2,5,10))) %>%
  dplyr::mutate(sim_group=row_number())

#combine the two data frames and add the number of times to run each simulation
complete_df <- crossing(fixed_df, varying_df, sim_rep=c(1:3)) %>%
  dplyr::mutate(sim_unique_id=row_number())
complete_df  #one row per simulation

#for the first 3 simulations
#do_simulation_type2(complete_df[1:3,])

#do in parallel with more sims
parallel_sim_df <- crossing(fixed_df, varying_df, sim_rep=c(1:40)) %>%
  dplyr::mutate(sim_unique_id=row_number(),
                batch=sample(1:40, n(), replace = TRUE))
parallel_sim_df #note that the batch column determines how the simulations are grouped

#going to make use of subset_apply function which is used as follows:
#res <- subset_apply(k=1, df=parallel_sim_df, my_fun=do_simulation_type2)

library(batchtools)

#on Mac
bjwork_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
bjfiles_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
dir.create(bjwork_dir)
#dir.create(bjfiles_dir)
bj_resources <- list()

#on cluster
#BatchJobs::makeClusterFunctionsTorque(template.file='templ.txt')
#bj_resources <- list(nodes = 1, ppn=1, walltime="00:00:20:00", vmem='4G')

#create the registry
reg <- makeRegistry(packages=c('pgxsim', 'tidyverse'), #packages to include
                    work.dir = bjwork_dir,
                    file.dir = bjfiles_dir,
                    make.default=FALSE)

reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = 8)

#map
batchMap(reg=reg, fun=subset_apply, k=1:max(parallel_sim_df$batch),
         more.args=list(df=parallel_sim_df, my_fun=do_simulation_type2))
#submitJobs(reg)
submitJobs(reg=reg, resources = bj_resources)
getStatus(reg = reg)
done <- findDone(reg)
job_tab <- getJobTable(reg=reg)


#gather results and bind into a dataframe
parallel_res <- reduceResultsList(reg=reg, ids=findDone(reg=reg))
parallel_res_df <- bind_rows(parallel_res) %>%
  inner_join(parallel_sim_df, by='sim_unique_id')

#tidy up
removeRegistry(reg=reg, wait=0)

#plot
ggplot(parallel_res_df, aes(as.factor(round(beta,2)), -log10(test_pval), colour=method)) +
  geom_boxplot(outlier.size=0) +
  facet_grid(prop~n) +
  theme_bw()

