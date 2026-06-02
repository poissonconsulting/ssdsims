# Run a Scenario with the Baseline Loop Runner

Executes the four task tables in dependency order - `sample`, `data`,
`fit`, then `hc` - by looping over each table with
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) and
looking up each task's parent result by the parent's `<step>_id` foreign
key. The runner does no task expansion of its own (it consumes
[`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md));
it just threads outputs forward and returns the collected per-step
results.

## Usage

``` r
ssd_run_scenario_baseline(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

A named list with `sample`, `data`, `fit`, and `hc` elements: each the
corresponding task table augmented with a list column of per-task
results (`sample` draws, `data` truncations, `fits` objects, and `hc`
tibbles).

## Details

This is the no-frills baseline: it runs in-process, with **no**
`targets` dependency, **no** shard grouping or `partition_by`, and
**no** Parquet I/O. It is also **not** reproducible - no per-task RNG
seeding happens here (that arrives with the `state-primitives` roadmap
step); pin the ambient RNG (e.g.
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html))
for deterministic draws.

The scenario retains the data frames it was built from, so the runner
reads them directly - no separate `data` argument. `min_pmix` names are
resolved against `ssdtools` until the registry roadmap step lands.

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = "lnorm"
)
withr::with_seed(42L, {
  out <- ssd_run_scenario_baseline(scenario)
})
out$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 1
#> # A tibble: 1 × 18
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 10 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>, hc <list>
```
