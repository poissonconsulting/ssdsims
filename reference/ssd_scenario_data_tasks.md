# Derive the data Task Table from a Scenario

Crosses each sample-task identity (`dataset`, `sim`, `replace`) with the
scenario's `nrow` values: each `data` task is the `head(sample, nrow)`
truncation of its parent sample. Unlike the draw, this step is RNG-free,
so `nrow` is an ordinary cross-join axis here (one truncation per size)
without duplicating the underlying draw (`TARGETS-DESIGN.md` section 5).

## Usage

``` r
ssd_scenario_data_tasks(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

An `ssdsims_tasks` object recording the `"data"` step, with one row per
`(dataset, sim, replace, nrow)` cell.

## Details

Each row carries a `data_id` primary key and a `sample_id` foreign key
referencing its parent sample task.

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 2L,
  nrow = c(5L, 10L),
  seed = 42L
)
ssd_scenario_data_tasks(scenario)
#> <ssdsims_tasks: data>
#>   axes:  dataset, sim, replace, nrow
#>   tasks: 4
#> # A tibble: 4 × 6
#>   dataset      sim replace  nrow data_id                               sample_id
#>   <chr>      <int> <lgl>   <int> <chr>                                 <chr>    
#> 1 ccme_boron     1 FALSE       5 dataset=ccme_boron/sim=1/replace=FAL… dataset=…
#> 2 ccme_boron     1 FALSE      10 dataset=ccme_boron/sim=1/replace=FAL… dataset=…
#> 3 ccme_boron     2 FALSE       5 dataset=ccme_boron/sim=2/replace=FAL… dataset=…
#> 4 ccme_boron     2 FALSE      10 dataset=ccme_boron/sim=2/replace=FAL… dataset=…
```
