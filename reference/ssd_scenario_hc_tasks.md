# Derive the hc Task Table from a Scenario

Crosses each fit-task identity with each row of the scenario's `hc`
argument grid (`nboot`, `ci_method`, `parametric`). The scenario's
scalar `ci` flag and the `est_method` setting are applied uniformly to
every hc row - neither is a cross-join axis (both are absent from
`task_axes("hc")`); `ci` rides as a carried column and every requested
`est_method` is summarised within each task from its single bootstrap
sample set. When `ci = FALSE`, the bootstrap-only knobs (`nboot`,
`ci_method`, `parametric`) are canonically `NA` and there is no fan-out
axis, so the grid is exactly one hc row per fit task; when `ci = TRUE`,
the grid fans out across `nboot x ci_method x parametric`.

## Usage

``` r
ssd_scenario_hc_tasks(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

An `ssdsims_tasks` object recording the `"hc"` step, with one row per
fit-task identity crossed with the hc grid.

## Details

Each row carries an `hc_id` primary key and a `fit_id` foreign key
referencing its parent fit task.

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
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
