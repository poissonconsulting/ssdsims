# ssdsims

The goal of ssdsims is to facilitate simulation studies with species
sensitivity distribution data.

## Installation

``` r

library(ssdsims)

scenario <- ssd_define_scenario(
  ssd_scenario_data(ssddata::ccme_boron),
  nsim = 2L,
  seed = 42L,
  nrow = c(6L, 10L),
  ci = TRUE,
  nboot = 10L,
  ci_method = c("multi_fixed", "weighted_samples")
)
scenario
#> <ssdsims_scenario>
#>   seed:     42
#>   nsim:     2
#>   datasets: ccme_boron
#>   nrow:     6, 10
#>   replace:  TRUE
#>   nrow_max: 1000 (setting)
#>   fit grid:
#>     rescale: FALSE
#>     computable: FALSE
#>     at_boundary_ok: TRUE
#>     min_pmix: ssd_min_pmix
#>     range_shape1: {0.05, 20}
#>     range_shape2: {0.05, 20}
#>     dists: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull (setting)
#>   hc grid:
#>     est_method: multi (setting)
#>     proportion: 0.05 (setting)
#>     ci: TRUE (setting)
#>     nboot: 10
#>     ci_method: multi_fixed, weighted_samples
#>     parametric: TRUE
#>     samples: FALSE (setting)
#>   distsets:
#>     BCANZ: gamma, lgumbel, llogis, lnorm, lnorm_lnorm, weibull
#>   partition_by:
#>     sample: dataset, sim, replace
#>     fit: dataset, sim, nrow, rescale
#>     hc: dataset, sim
#>   bundle:
#>     sample: 
#>     fit: replace, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>     hc: replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset

ssd_run_scenario_baseline(scenario)
#> $sample
#> <ssdsims_tasks: sample>
#>   axes:  dataset, sim, replace
#>   tasks: 2
#> # A tibble: 2 × 5
#>   dataset      sim replace sample_id                             sample      
#>   <chr>      <int> <lgl>   <chr>                                 <named list>
#> 1 ccme_boron     1 TRUE    dataset=ccme_boron/sim=1/replace=TRUE <tibble>    
#> 2 ccme_boron     2 TRUE    dataset=ccme_boron/sim=2/replace=TRUE <tibble>    
#> 
#> $fit
#> <ssdsims_tasks: fit>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>   tasks: 4
#> # A tibble: 4 × 16
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     2 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 4 ccme_boron     2 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, fit_id <chr>,
#> #   sample_id <chr>, fits <list>, .start <dttm>, .end <dttm>, .host <chr>
#> 
#> $hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
#>   tasks: 8
#> # A tibble: 8 × 20
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> 4 ccme_boron     1 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> 5 ccme_boron     2 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 6 ccme_boron     2 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 7 ccme_boron     2 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> 8 ccme_boron     2 TRUE       10 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 12 more variables: range_shape1 <list>, range_shape2 <list>, nboot <int>,
#> #   ci_method <chr>, parametric <lgl>, distset <chr>, hc_id <chr>,
#> #   fit_id <chr>, hc <list>, .start <dttm>, .end <dttm>, .host <chr>
```
