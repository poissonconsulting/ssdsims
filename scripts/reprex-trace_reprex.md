``` r
options(ssdsims.trace = TRUE)
library(ssdsims)

# Second example scenario from scripts/example.R (faster path: ci = FALSE).
res <- ssd_run_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 6L, 10L, 20L, 50L),
  proportion = c(0.01, 0.05, 0.1, 0.2),
  est_method = c("arithmetic", "geometric", "multi"),
  ci = FALSE,
  parametric = TRUE,
  nboot = c(1, 5, 10, 50, 100, 500),
  samples = FALSE,
  delta = Inf,
  .progress = FALSE
)
#> [ssdsims] ssd_sim_data.df      nsim=2 nrow=<integer[5]> replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=FALSE kind=Mersenne-Twister/Inversion/Rejection
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=5 replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=FALSE kind=Mersenne-Twister/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=9f442733 last=9f442733
#> [ssdsims] ssd_sim_data.states  n=1 first=9f442733 last=9f442733
#> [ssdsims] slice_sample_state   n=5 replace=FALSE state=9f442733
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=6 replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] ssd_sim_data.states  n=1 first=94e47d37 last=94e47d37
#> [ssdsims] slice_sample_state   n=6 replace=FALSE state=94e47d37
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=10 replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] ssd_sim_data.states  n=1 first=94e47d37 last=94e47d37
#> [ssdsims] slice_sample_state   n=10 replace=FALSE state=94e47d37
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=20 replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] ssd_sim_data.states  n=1 first=94e47d37 last=94e47d37
#> [ssdsims] slice_sample_state   n=20 replace=FALSE state=94e47d37
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=50 replace=FALSE start_sim=1 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] ssd_sim_data.states  n=1 first=94e47d37 last=94e47d37
#> [ssdsims] slice_sample_state   n=50 replace=FALSE state=94e47d37
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=5 replace=FALSE start_sim=2 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] ssd_sim_data.states  n=1 first=f566e51c last=f566e51c
#> [ssdsims] slice_sample_state   n=5 replace=FALSE state=f566e51c
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=6 replace=FALSE start_sim=2 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] ssd_sim_data.states  n=1 first=f566e51c last=f566e51c
#> [ssdsims] slice_sample_state   n=6 replace=FALSE state=f566e51c
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=10 replace=FALSE start_sim=2 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] ssd_sim_data.states  n=1 first=f566e51c last=f566e51c
#> [ssdsims] slice_sample_state   n=10 replace=FALSE state=f566e51c
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=20 replace=FALSE start_sim=2 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] ssd_sim_data.states  n=1 first=f566e51c last=f566e51c
#> [ssdsims] slice_sample_state   n=20 replace=FALSE state=f566e51c
#> [ssdsims] ssd_sim_data.df      nsim=1 nrow=50 replace=FALSE start_sim=2 stream=1 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] ssd_sim_data.states  n=1 first=f566e51c last=f566e51c
#> [ssdsims] slice_sample_state   n=50 replace=FALSE state=f566e51c
#> [ssdsims] ssd_fit_dists_sims   n_in=10 seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] fit_dists_seed       sim=1 stream=1 seed=NULL state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] fit_dists_seed       sim=1 stream=1 seed=NULL state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] fit_dists_seed       sim=1 stream=1 seed=NULL state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] fit_dists_seed       sim=1 stream=1 seed=NULL state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] fit_dists_seed       sim=1 stream=1 seed=NULL state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] fit_dists_seed       sim=2 stream=1 seed=NULL state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] fit_dists_seed       sim=2 stream=1 seed=NULL state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] fit_dists_seed       sim=2 stream=1 seed=NULL state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] fit_dists_seed       sim=2 stream=1 seed=NULL state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] fit_dists_seed       sim=2 stream=1 seed=NULL state=f566e51c
#> [ssdsims] ssd_hc_sims          n_in=10 ci=FALSE seed=NULL
#> [ssdsims] .Random.seed         exists=TRUE kind=L'Ecuyer-CMRG/Inversion/Rejection
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=1 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=5 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=10 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=50 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=100 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=1
#> [ssdsims] get_states.out       first=94e47d37 last=94e47d37
#> [ssdsims] hc_seed              sim=1 stream=1 seed=NULL nboot=500 ci=FALSE state=94e47d37
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=1 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=5 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=10 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=50 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=100 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c
#> [ssdsims] get_states           seed=NULL nsim=1 stream=1 start_sim=2
#> [ssdsims] get_states.out       first=f566e51c last=f566e51c
#> [ssdsims] hc_seed              sim=2 stream=1 seed=NULL nboot=500 ci=FALSE state=f566e51c

dim(res)
#> [1] 180  17
head(res, 3)
#> # A tibble: 3 × 17
#>     sim stream replace  nrow data     rescale computable at_boundary_ok min_pmix
#>   <int>  <int> <lgl>   <int> <list>   <lgl>   <lgl>      <lgl>          <list>  
#> 1     1      1 FALSE       5 <tibble> FALSE   FALSE      TRUE           <fn>    
#> 2     1      1 FALSE       5 <tibble> FALSE   FALSE      TRUE           <fn>    
#> 3     1      1 FALSE       5 <tibble> FALSE   FALSE      TRUE           <fn>    
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fits <list>,
#> #   nboot <dbl>, est_method <chr>, ci_method <chr>, parametric <lgl>, hc <list>
```
