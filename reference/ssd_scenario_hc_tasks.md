# Derive the hc Task Table from a Scenario

Crosses each fit-task identity with each row of the scenario's `hc`
argument grid (`nboot`, `est_method`, `ci_method`, `parametric`). The
expansion honours the construction-time `ci = FALSE` collapse
(`TARGETS-DESIGN.md` section 1.2): rows where `ci = FALSE` are not
multiplied across the bootstrap-only knobs (`nboot`, `ci_method`,
`parametric`), which are stored as `NA`, while `ci = TRUE` rows fan out
across the full grid.

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
fit-task identity crossed with the (collapsed) hc grid.

## Details

Each row carries an `hc_id` primary key and a `fit_id` foreign key
referencing its parent fit task.

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  seed = 42L,
  ci = c(FALSE, TRUE),
  nboot = c(10L, 100L)
)
ssd_scenario_hc_tasks(scenario)
#> <ssdsims_tasks: hc>
#>   axes:  dataset, sim, replace, nrow, rescale, computable, at_boundary_ok, min_pmix, range_shape1, range_shape2, ci, nboot, est_method, ci_method, parametric
#>   tasks: 6
#> # A tibble: 6 × 17
#>   dataset      sim replace  nrow rescale computable at_boundary_ok min_pmix    
#>   <chr>      <int> <lgl>   <int> <lgl>   <lgl>      <lgl>          <chr>       
#> 1 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 2 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 3 ccme_boron     1 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 4 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 5 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> 6 ccme_boron     2 FALSE       6 FALSE   FALSE      TRUE           ssd_min_pmix
#> # ℹ 9 more variables: range_shape1 <list>, range_shape2 <list>, ci <lgl>,
#> #   nboot <int>, est_method <chr>, ci_method <chr>, parametric <lgl>,
#> #   hc_id <chr>, fit_id <chr>
```
