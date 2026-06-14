# Expand a Scenario into Task Tables

The canonical expansion entry point (`TARGETS-DESIGN.md` section
1/section 2): `ssd_scenario_tasks()` derives the `sample`, `fit`, and
`hc` task tables from a scenario in one call and bundles them into an
`ssdsims_task_set`. The per-step derivations
(`ssd_scenario_sample_tasks()`, `ssd_scenario_fit_tasks()`,
`ssd_scenario_hc_tasks()`) remain available for callers that need a
single table; each is equivalent to `ssd_scenario_tasks(scenario, step)`
for the matching step.

All derivations are RNG-free: they perform no random-number generation
and add no `seed`/`primer`/`stream` columns (those arrive in later
roadmap steps; see `TARGETS-DESIGN.md` section 2). Each row carries a
path-style `<step>_id` primary key (the Hive partition path) and, for
non-root steps, its parent step's `<parent>_id` as a joinable foreign
key, so a child task references its parent by a single column.

## Usage

``` r
ssd_scenario_tasks(scenario, step = NULL)

ssd_scenario_sample_tasks(scenario)

ssd_scenario_fit_tasks(scenario)

ssd_scenario_hc_tasks(scenario)
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
the single `ssdsims_tasks` table for that step. Each `ssdsims_tasks`
table is a classed tibble recording one step, with one row per cell of
that step's cross-join.

## Functions

- `ssd_scenario_sample_tasks()`: Derive just the `sample` task table:
  one row per cell of the cross-join of the scenario's dataset names,
  replicate index (`1:nsim`), and `replace` values, keyed by
  `sample_id`. Each row is the single random draw of `n_max = max(nrow)`
  rows that every `nrow` value sub-truncates (`TARGETS-DESIGN.md`
  section 5), so `nrow` is **not** a sample axis - the draw is shared -
  and `n_max` is carried as an ordinary integer column.

- `ssd_scenario_fit_tasks()`: Derive just the `fit` task table: cross
  each sample-task identity (`dataset`, `sim`, `replace`) with the
  scenario's `nrow` values and each row of the scenario's `fit` argument
  grid (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name,
  `range_shape1`, `range_shape2`). `nrow` is a genuine `fit` cross-join
  axis: the `fit` step truncates its parent sample inline
  (`head(sample, nrow)`, RNG-free) before fitting, so the shared draw is
  sub-truncated without a separate `data` step (`TARGETS-DESIGN.md`
  section 5). `min_pmix` is referenced by name, not by function value
  (`TARGETS-DESIGN.md` section 1.1). Each row carries a `fit_id` primary
  key and a `sample_id` foreign key referencing its parent sample task.

- `ssd_scenario_hc_tasks()`: Derive just the `hc` task table: cross each
  fit-task identity with each row of the scenario's `hc` argument grid
  (`nboot`, `ci_method`, `parametric`). The scenario's scalar `ci` flag
  and the `est_method` setting are applied uniformly to every hc row -
  neither is a cross-join axis; `ci` rides as a carried column and every
  requested `est_method` is summarised within each task from its single
  bootstrap sample set. When `ci = FALSE` the bootstrap-only knobs
  (`nboot`, `ci_method`, `parametric`) are canonically `NA` and there is
  no fan-out axis, so the grid is exactly one hc row per fit task; when
  `ci = TRUE` the grid fans out across `nboot x ci_method x parametric`.
  Each row carries an `hc_id` primary key and a `fit_id` foreign key
  referencing its parent fit task.

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 3L, seed = 42L)
tasks <- ssd_scenario_tasks(scenario)
tasks
#> <ssdsims_task_set>
#>   sample tasks: 3
#>   fit    tasks: 3
#>   hc     tasks: 3
tasks$hc
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric
#>   tasks: 3
#> # A tibble: 3 × 16
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     3 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, ci_method <chr>, parametric <lgl>, hc_id <chr>, fit_id <chr>
ssd_scenario_tasks(scenario, "hc")
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric
#>   tasks: 3
#> # A tibble: 3 × 16
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     3 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, ci_method <chr>, parametric <lgl>, hc_id <chr>, fit_id <chr>
ssd_scenario_sample_tasks(scenario)
#> <ssdsims_tasks: sample>
#>   axes:  dataset, sim, replace
#>   tasks: 3
#> # A tibble: 3 × 5
#>   dataset      sim replace n_max sample_id                             
#>   <chr>      <int> <lgl>   <int> <chr>                                 
#> 1 ccme_boron     1 FALSE       6 dataset=ccme_boron/sim=1/replace=FALSE
#> 2 ccme_boron     2 FALSE       6 dataset=ccme_boron/sim=2/replace=FALSE
#> 3 ccme_boron     3 FALSE       6 dataset=ccme_boron/sim=3/replace=FALSE
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 3L,
  seed = 42L,
  rescale = c(FALSE, TRUE)
)
ssd_scenario_fit_tasks(scenario)
#> <ssdsims_tasks: fit>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2
#>   tasks: 6
#> # A tibble: 6 × 12
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     1 FALSE       6 TRUE    FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 4 ccme_boron     2 FALSE       6 TRUE    FALSE      TRUE           ssd_min_pmix
#> 5 ccme_boron     3 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 6 ccme_boron     3 FALSE       6 TRUE    FALSE      TRUE           ssd_min_pmix
#> # ℹ 4 more variables: range_shape1 <list>, range_shape2 <list>, fit_id <chr>,
#> #   sample_id <chr>
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  ci = TRUE,
  nboot = c(10L, 100L)
)
ssd_scenario_hc_tasks(scenario)
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, nboot, ci_method, parametric
#>   tasks: 4
#> # A tibble: 4 × 16
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 4 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 8 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, ci_method <chr>, parametric <lgl>, hc_id <chr>, fit_id <chr>
```
