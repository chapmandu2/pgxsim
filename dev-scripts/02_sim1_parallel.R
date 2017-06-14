library(pgxsim)
library(tidyverse)

#prepare a data frame
fixed_df <- data_frame(type='discrete', mu=1, lb=0.001, ub=30, ndoses=10, nreps=3, sd_prop=0.3, sd_add=0.15)
varying_df <- crossing(sd=c(0.1, 0.5, 2), n=c(50, 200, 800), prop=c(0.05,0.1,0.2), beta=log10(c(2,5,10))) %>%
#varying_df <- crossing(sd=c(0.1,2), n=c(50, 200), prop=c(0.05,0.2), beta=log10(c(2,5,10))) %>%
  dplyr::mutate(sim_group=row_number())
complete_df <- crossing(fixed_df, varying_df, sim_rep=c(1:10)) %>%
  dplyr::mutate(sim_unique_id=row_number(),
                batch=sample(1:32, n(), replace = TRUE))

#test1 <- do_simulation_type1(dplyr::sample_n(complete_df, 5))
#test2 <- subset_apply(6, complete_df, do_simulation_type1)

# process in parallel using batchtools -------------------------------------

library(BatchJobs)
getConfig()

if(grepl('apple', sessionInfo()$platform)) {
  #control_df <- control_df %>% dplyr::filter(i==1) #%>% dplyr::slice(1:3)
  setConfig(cluster.functions=makeClusterFunctionsMulticore(ncpus=8))
  bjwork_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
  bjfiles_dir <- file.path(tempdir(), sample(LETTERS, 10) %>% paste(., collapse=''))
  dir.create(bjwork_dir)
  dir.create(bjfiles_dir)
  bj_resources <- list()

} else {
  bjwork_dir <-  getwd()
  bj_resources <- list(nodes = 1, ppn=2, walltime="00:00:20:00", vmem='16G')
}


#create the registry
reg <- makeRegistry(id="BatchJobs",
                    packages=c('pgxsim', 'tidyverse'), #packages to include
                    work.dir = bjwork_dir,
                    file.dir = bjfiles_dir)

#map
batchMap(reg, fun=subset_apply, k=1:max(complete_df$batch), more.args=list(df=complete_df, my_fun=do_simulation_type1))
#submitJobs(reg)
submitJobs(reg, resources = bj_resources)
#waitForJobs(reg)
showStatus(reg)
done <- findDone(reg)
job_info <- getJobInfo(reg)

#gather results and bind into a dataframe
res <- reduceResultsList(reg, findDone(reg))
res_df <- bind_rows(res) %>%
  dplyr::select(sim_unique_id, term, estimate, std.error, statistic, p.value, method) %>%
  inner_join(complete_df, by='sim_unique_id')

#tidy up
removeRegistry(reg, ask='no')

# explore results
#estimates
ggplot(res_df, aes(as.factor(round(beta,2)), estimate, colour=method)) +
  geom_boxplot(outlier.size = 0) +
  facet_grid(prop~sd+n) + ylim(-1,2) +
  theme_bw()

#p values
ggplot(res_df, aes(as.factor(round(beta,2)), -log10(p.value), colour=method)) +
  geom_boxplot(outlier.size = 0) +
  facet_grid(prop~sd+n) +
  theme_bw()

#proportion of times that p<0.05
sig_calc <- res_df %>%
  dplyr::mutate(p_sig=p.value<=0.05,
                rci_sig=abs(1.98*std.error/estimate)<=1) %>%
  dplyr::group_by(sim_group, method, sd, n, prop, beta) %>%
  dplyr::summarise(p_sig=mean(p_sig),
            rci_sig=mean(rci_sig))


#as previous
ggplot(sig_calc, aes(x=n, y=p_sig, colour=method, linetype=method)) +
  geom_line(alpha=0.8, size=1) +
  facet_grid(prop~sd+round(beta,1)) +
  theme_bw()

ggplot(sig_calc, aes(x=n, y=rci_sig, colour=method, linetype=method)) +
  geom_line(alpha=0.8, size=1) +
  facet_grid(prop~sd+round(beta,1)) +
  theme_bw()

