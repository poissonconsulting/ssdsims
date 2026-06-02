# Derive the sample Task Table from a Scenario

Expands an `ssdsims_scenario` into the `sample` task table: one row per
cell of the cross-join of the scenario's dataset names, replicate index
(`1:nsim`), and `replace` values. Each row is the single random draw of
`n_max = max(nrow)` rows that every `nrow` value sub-truncates
(`TARGETS-DESIGN.md` section 5), so `nrow` is **not** a sample axis -
the draw is shared. `n_max` is carried as an ordinary integer column.
The derivation performs no random-number generation and adds no
`seed`/`primer`/`stream` columns (those arrive in later roadmap steps;
see `TARGETS-DESIGN.md` section 2).

## Usage

``` r
ssd_scenario_sample_tasks(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

An `ssdsims_tasks` object (a classed tibble recording the `"sample"`
step) with one row per `(dataset, sim, replace)` cell, a `sample_id`
key, and a carried `n_max` column.

## Details

Each row carries a path-style `sample_id` primary key.

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 3L, seed = 42L)
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
```
