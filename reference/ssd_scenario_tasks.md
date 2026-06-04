# Expand a Scenario into all Three Task Tables

The canonical expansion entry point (`TARGETS-DESIGN.md` section
1/section 2): derives the `sample`, `fit`, and `hc` task tables from a
scenario in one call and bundles them into an `ssdsims_task_set`. The
per-step derivations
([`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_tasks.md),
[`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_tasks.md),
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_tasks.md))
remain available for callers that need a single table.

## Usage

``` r
ssd_scenario_tasks(scenario, step = NULL)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- step:

  Optional single step name (`"sample"`, `"fit"`, or `"hc"`). When
  supplied, returns just that step's `ssdsims_tasks` table (the same as
  the matching `ssd_scenario_*_tasks()`); when `NULL` (default) returns
  the full `ssdsims_task_set`.

## Value

An `ssdsims_task_set` object (a list with `sample`, `fit`, and `hc`
elements, each an `ssdsims_tasks` table), or - when `step` is supplied -
the single `ssdsims_tasks` table for that step.

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
tasks <- ssd_scenario_tasks(scenario)
tasks
#> <ssdsims_task_set>
#>   sample tasks: 3
#>   fit    tasks: 3
#>   hc     tasks: 3
tasks$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 3
#> # A tibble: 3 × 17
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     3 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 9 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>
ssd_scenario_tasks(scenario, "hc")
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 3
#> # A tibble: 3 × 17
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     3 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 9 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>
```
