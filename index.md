# ssdsims

ssdsims runs reproducible simulation studies for species sensitivity
distribution (SSD) models built on the
[ssdtools](https://bcgov.github.io/ssdtools/) package. You describe a
study **declaratively** as a *scenario*; ssdsims expands it into
per-step task tables, draws the data, fits the distributions, and
estimates the hazard concentrations. The same scenario runs three ways
over one execution core — an in-memory baseline, single-core
Hive-partitioned Parquet *shards*, and a
[targets](https://docs.ropensci.org/targets/)-based shard pipeline for
running in parallel or on a cluster — with byte-identical, reproducible
per-task results throughout.

New here? Start with
[`vignette("ssdsims")`](https://poissonconsulting.github.io/ssdsims/articles/ssdsims.html)
for the overview and a recommended reading order.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("poissonconsulting/ssdsims")

# or
# install.packages("remotes")
remotes::install_github("poissonconsulting/ssdsims")
```

## Usage

Assemble the data, declare a scenario, and run the in-memory baseline:

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

The scenario is purely declarative — it draws no random numbers and
depends on no `targets` — so the same object drives every runner. See
[`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.html)
for the scenario object and
[`vignette("sharded-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.html)
for the shard and `targets` pipeline.

## Capability map

| Area | Article |
|----|----|
| Define a scenario and expand it into `sample`/`fit`/`hc` task tables | [`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.html) |
| Run as Hive-partitioned Parquet shards and a `targets` pipeline | [`vignette("sharded-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.html) |
| Combine several scenarios into one ragged design | [`vignette("scenario-to-design")`](https://poissonconsulting.github.io/ssdsims/articles/scenario-to-design.html) |
| Run on a SLURM (or other) cluster | [`vignette("cluster-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.html) |
| Ship shards to cloud object storage | [`vignette("cloud-upload")`](https://poissonconsulting.github.io/ssdsims/articles/cloud-upload.html) |
| Estimate compute cost before a run | [`vignette("cost-estimation")`](https://poissonconsulting.github.io/ssdsims/articles/cost-estimation.html) |
| Analyse observed compute cost after a run | [`vignette("cost-analysis")`](https://poissonconsulting.github.io/ssdsims/articles/cost-analysis.html) |

Reproducible per-task RNG (the dqrng `pcg64` backend with per-task
primers) underpins all of the above; see
[`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.html).
