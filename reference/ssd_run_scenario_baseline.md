# Run a Scenario with the Baseline Loop Runner

Executes the three task tables in dependency order - `sample`, `fit`,
then `hc` - by looping over each table with
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) and
looking up each task's parent result by the parent's `<step>_id` foreign
key. The `fit` step truncates its parent sample inline
(`head(sample, nrow)`) before fitting. The runner does no task expansion
of its own (it consumes
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

A named list with `sample`, `fit`, and `hc` elements: each the
corresponding task table augmented with a list column of per-task
results (`sample` draws, `fits` objects, and `hc` tibbles).

## Details

This is the no-frills baseline: it runs in-process, with **no**
`targets` dependency, **no** shard grouping or `partition_by`, and
**no** Parquet I/O.

It **is** reproducible without an external seed. The runner opens one
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope and seeds each `sample`/`fit`/`hc` task exactly once through its
`*_data_task_primer()` wrapper, with `seed = scenario$seed` and a
per-task primer derived from the task's canonical identity
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over the `task_axes(step)` columns). Because each task's
`(seed, primer)` pair fully determines its RNG starting point, two runs
of a scenario with a fixed `seed` yield identical results, and a task's
result is independent of the order in which tasks run. These same
`*_data_task_primer()` wrappers are the per-task entry point a future
`targets` shard body and the replay helper (`TARGETS-DESIGN.md` §7)
reuse.

The scenario retains the data frames it was built from, so the runner
reads them directly - no separate `data` argument. `min_pmix` names are
resolved to their materialised functions off the scenario via
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)
(resolved once, at construction), not by a runtime `ssdtools`/global-env
search.

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = ssd_distset(lnorm = "lnorm")
)
out <- ssd_run_scenario_baseline(scenario)
out$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric, distset
#>   tasks: 1
#> # A tibble: 1 × 17
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 TRUE        6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 9 more variables: range_shape1 <list>, range_shape2 <list>, nboot <int>,
#> #   ci_method <chr>, parametric <lgl>, distset <chr>, hc_id <chr>,
#> #   fit_id <chr>, hc <list>
```
