## Example scenario

library(ssdsims)

sims <- ssd_sim_data(ssddata::ccme_boron)
fits <- ssd_fit_dists_sims(sims)
hcs <- ssd_hc_sims(fits)
print(hcs)

ssd_run_scenario(ssddata::ccme_boron, 
                 nsim=2L,
                 nrow=c(5L, 6L, 10L, 20L, 50L),
                 proportion = c(0.01, 0.05, 0.1, 0.2),
                 ci=TRUE, 
                 ci_method=c("multi_fixed","multi_free", "weighted_samples", "MACL"),
                 est_method=c("multi", "arithmetic", "geometric"),
                 parametric=c(TRUE, FALSE),
                 nboot=c(1, 5, 10, 50, 100, 500), # * 100,
                 samples=TRUE,
                 delta=Inf)
