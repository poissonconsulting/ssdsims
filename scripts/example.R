## Example scenario
## I don't need samples = TRUE necessarily, but I am keen to understand the mechanism by which these could be kept.

library(ssdsims)

sims <- ssd_sim_data(ssddata::ccme_boron)
fits <- ssd_fit_dists_sims(sims)
hcs <- ssd_hc_sims(fits)
print(hcs)

ssd_run_scenario(ssddata::ccme_boron, 
                 nsim = 2L,
                 nrow = c(5L, 6L, 10L, 20L, 50L),
                 proportion = c(0.01, 0.05, 0.1, 0.2),
                 ci_method = c("arithmetic_samples", "geometric_samples", "GMACL", "GMAW1", 
                 "GMAW2", "MACL", "MAW1", "MAW2", "multi_fixed", "multi_free", 
                 "weighted_samples"),
                 nboot=c(1, 5, 10, 50, 100, 500), # * 100,
                 ci = TRUE, 
                 est_method = "multi",
                 parametric = TRUE,
                 samples=TRUE,
                 delta=Inf)
