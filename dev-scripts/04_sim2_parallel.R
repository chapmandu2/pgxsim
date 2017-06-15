library(pgxsim)
library(tidyverse)

#prepare a data frame
fixed_df <- data_frame(type='discrete', mu=.5, lb=-4, ub=Inf, minconc=0.001, maxconc=30, ndoses=7, nreps=1, sd_prop=0.3, sd_add=0.5)
varying_df <- crossing(sd=c(.4, 1), n=c(50, 200, 800), prop=c(0.05,0.1,0.2), beta=-c(.5, 1)) %>%
  #varying_df <- crossing(sd=1, n=c(50, 200), prop=c(0.05,0.1, 0.2), beta=-c(.5, 1)) %>%
  dplyr::mutate(sim_group=row_number())
complete_df <- crossing(fixed_df, varying_df, sim_rep=c(1:40)) %>%
  dplyr::mutate(sim_unique_id=row_number(),
                batch=sample(1:128, n(), replace = TRUE))

test_df <- dplyr::sample_n(complete_df, 5)
#test1 <- do_simulation_type2(dplyr::sample_n(complete_df, 5))
#test2 <- subset_apply(6, complete_df, do_simulation_type2)

#complete_df %>% dplyr::filter(batch==6) %>% dplyr::slice(10) %>% do_simulation_type2()

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
batchMap(reg, fun=subset_apply, k=1:max(complete_df$batch),
         more.args=list(df=complete_df, my_fun=do_simulation_type2))
#submitJobs(reg)
submitJobs(reg, resources = bj_resources)
#waitForJobs(reg)
showStatus(reg)
done <- findDone(reg)
job_info <- getJobInfo(reg)

#gather results and bind into a dataframe
res <- reduceResultsList(reg, findDone(reg))
res_df <- bind_rows(res) %>%
  inner_join(complete_df, by='sim_unique_id')

#tidy up
removeRegistry(reg, ask='no')

# explore results
#estimates
ggplot(res_df, aes(as.factor(round(beta,2)), beta_estimate, colour=method)) +
  geom_boxplot(outlier.size = 0) +
  facet_grid(prop~sd+n) + ylim(-1,2) +
  theme_bw()

#p values
ggplot(res_df, aes(as.factor(round(beta,2)), -log10(beta_pval), colour=method)) +
  geom_boxplot(outlier.size = 0) +
  facet_grid(prop~sd+n) +
  theme_bw()

#test p values
ggplot(res_df, aes(as.factor(round(beta,2)), -log10(test_pval), colour=method)) +
  geom_boxplot(outlier.size = 0) +
  facet_grid(prop~sd+n) +
  theme_bw()

#proportion of times that p<0.05
sig_calc <- res_df %>%
  dplyr::mutate(betap_sig=beta_pval<=0.05,
                testp_sig=test_pval<=0.05,
                rci_sig=abs(1.98*beta_std_err/beta_estimate)<=1) %>%
  dplyr::group_by(sim_group, method, sd, n, prop, beta) %>%
  dplyr::summarise(betap_sig=mean(betap_sig),
                   testp_sig=mean(testp_sig),
                   rci_sig=mean(rci_sig))


#as previous
ggplot(sig_calc, aes(x=n, y=betap_sig, colour=method, linetype=method)) +
  geom_line(alpha=0.8, size=1) +
  facet_grid(prop~sd+round(beta,1)) +
  theme_bw()

ggplot(sig_calc, aes(x=n, y=testp_sig, colour=method, linetype=method)) +
  geom_line(alpha=0.8, size=1) +
  facet_grid(prop~sd+round(beta,1)) +
  theme_bw()

ggplot(sig_calc, aes(x=n, y=rci_sig, colour=method, linetype=method)) +
  geom_line(alpha=0.8, size=1) +
  facet_grid(prop~sd+round(beta,1)) +
  theme_bw()

