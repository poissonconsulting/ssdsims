# Get Started with ssdsims

``` r

library(ssdsims)
```

## What ssdsims is for

A species sensitivity distribution (SSD) study asks how an estimate — a
hazard concentration, say — behaves as you vary the things you control:
the sample size, the number of simulations, the distributions you fit,
the bootstrap settings. Answering that means running the same
fit-and-estimate pipeline across a grid of settings, reproducibly, often
thousands of times.

ssdsims runs that study from a **declarative scenario**. You describe
the study once — a seed, the simulation count, the sample sizes, the
dataset *names*, and the fit/hc argument grids — and ssdsims expands it
into per-step task tables, draws the data, fits the distributions (via
[ssdtools](https://bcgov.github.io/ssdtools/)), and estimates the hazard
concentrations. The scenario itself draws no random numbers and writes
nothing, so it serialises to a compact manifest and the work it expands
to is a pure function of the scenario.

## Two tracks

The documentation splits into two tracks. Most users only need the
first.

      BUILD AND RUN ─────────────────────────────────────────────────────────
        vignette("defining-a-scenario")   define a scenario, expand task tables
                │
                ▼
        vignette("sharded-pipeline")      run it: in-memory → Parquet shards
                │                          → a targets pipeline (parallel)
                ├──► vignette("scenario-to-design")   combine scenarios (ragged)
                ├──► vignette("cluster-pipeline")     run on a SLURM cluster
                └──► vignette("cloud-upload")         ship shards to object storage

      PREDICT AND MEASURE COST ──────────────────────────────────────────────
        vignette("cost-estimation")       predict compute cost before a run
                │
                ▼
        vignette("cost-analysis")         measure observed cost after a run

A scenario can fan out into a multi-day run with no warning, so the cost
track lets you size a run before launching it and measure where the time
actually went afterwards. The two tracks meet at the scenario: both read
the same object.

## A recommended reading order

1.  **[`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)**
    — the scenario object end to end: assembling data, declaring the
    scenario, expanding the task tables, and the in-memory baseline
    runner. Start here.
2.  **[`vignette("sharded-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/sharded-pipeline.md)**
    — the same scenario as Hive-partitioned Parquet shards and a
    `targets` pipeline; the central “two drivers, one core,
    byte-identical” idea.
3.  Then, as your study grows, branch into
    **[`vignette("scenario-to-design")`](https://poissonconsulting.github.io/ssdsims/articles/scenario-to-design.md)**
    (combine scenarios),
    **[`vignette("cluster-pipeline")`](https://poissonconsulting.github.io/ssdsims/articles/cluster-pipeline.md)**
    (SLURM), and
    **[`vignette("cloud-upload")`](https://poissonconsulting.github.io/ssdsims/articles/cloud-upload.md)**
    (object storage).
4.  To size and audit compute, read
    **[`vignette("cost-estimation")`](https://poissonconsulting.github.io/ssdsims/articles/cost-estimation.md)**
    before a run and
    **[`vignette("cost-analysis")`](https://poissonconsulting.github.io/ssdsims/articles/cost-analysis.md)**
    after one.

## A 30-second on-ramp

Assemble the data with
[`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md),
declare the study with
[`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md),
and run the in-memory baseline with
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md):

``` r

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
```

``` r

out <- ssd_run_scenario_baseline(scenario)
out$hc
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

Because each task installs its own `(seed, primer)` under a scoped dqrng
backend, the result is reproducible from the scenario’s `seed` alone —
and identical whichever runner you choose.
[`vignette("defining-a-scenario")`](https://poissonconsulting.github.io/ssdsims/articles/defining-a-scenario.md)
picks up exactly here.
