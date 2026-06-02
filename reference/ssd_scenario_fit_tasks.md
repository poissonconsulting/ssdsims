# Derive the fit Task Table from a Scenario

Crosses each data-task identity (`dataset`, `sim`, `replace`, `nrow`)
with each row of the scenario's `fit` argument grid (`rescale`,
`computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`,
`range_shape2`). Parent-identity columns are preserved verbatim so the
table can be grouped directly downstream. `min_pmix` is referenced by
name, not by function value (`TARGETS-DESIGN.md` section 1.1).

## Usage

``` r
ssd_scenario_fit_tasks(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

An `ssdsims_tasks` object recording the `"fit"` step, with one row per
data-task identity crossed with the fit grid.

## Details

Each row carries a `fit_id` primary key and a `data_id` foreign key
referencing its parent data task.

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
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
#> #   data_id <chr>
```
