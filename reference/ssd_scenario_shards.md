# Group Tasks into Shards

Group a step's per-task table into a per-shard table: one row per
`partition_by` **path** cell, carrying the path-axis columns (the
`tar_map` target-name suffix and Hive path) and a `tasks` list-column of
that cell's task rows. Each task row is decorated with
`seed = scenario$seed` and its per-task `primer`
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over the step's `task_axes()`); the decoration is RNG-free (a pure hash,
not a draw), so the bare task tables
([`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md))
keep their no-`(seed, primer)` contract. The result is the `values` a
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)
consumes to mint one target per shard.

## Usage

``` r
ssd_scenario_sample_shards(scenario)

ssd_scenario_fit_shards(scenario)

ssd_scenario_hc_shards(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

A tibble with one row per shard of the step: the path-axis columns and a
`tasks` list-column. Suitable as `tarchetypes::tar_map(values = )`.

## Details

For `fit`/`hc` each task row in `tasks` also carries its parent step's
path-axis values and `<parent>_id`, so the runner opens the matching
parent shard by partition path.

## Functions

- `ssd_scenario_sample_shards()`: Group the `sample` tasks
  ([`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md))
  by `partition_by$sample`.

- `ssd_scenario_fit_shards()`: Group the `fit` tasks
  ([`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md))
  by `partition_by$fit`. Each task row in `tasks` carries its parent
  `sample` path-axis values and `sample_id`, so the runner opens the
  matching `sample` shard by partition path.

- `ssd_scenario_hc_shards()`: Group the `hc` tasks
  ([`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md))
  by `partition_by$hc`. Each task row in `tasks` carries its parent
  `fit` path-axis values and `fit_id`, so the runner opens the matching
  `fit` shard by partition path.

## See also

[`ssd_run_sample_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_step.md)
(the matching per-shard step runners).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 2L, seed = 42L)
ssd_scenario_sample_shards(scenario)
#> # A tibble: 2 × 4
#>   dataset      sim replace              tasks
#>   <chr>      <int> <lgl>   <list<tibble[,7]>>
#> 1 ccme_boron     1 FALSE              [1 × 7]
#> 2 ccme_boron     2 FALSE              [1 × 7]
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  rescale = c(FALSE, TRUE)
)
ssd_scenario_fit_shards(scenario)
#> # A tibble: 4 × 5
#>   dataset      sim  nrow rescale               tasks
#>   <chr>      <int> <int> <lgl>   <list<tibble[,14]>>
#> 1 ccme_boron     1     6 FALSE              [1 × 14]
#> 2 ccme_boron     1     6 TRUE               [1 × 14]
#> 3 ccme_boron     2     6 FALSE              [1 × 14]
#> 4 ccme_boron     2     6 TRUE               [1 × 14]
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  ci = TRUE
)
ssd_scenario_hc_shards(scenario)
#> # A tibble: 2 × 3
#>   dataset      sim               tasks
#>   <chr>      <int> <list<tibble[,18]>>
#> 1 ccme_boron     1            [1 × 18]
#> 2 ccme_boron     2            [1 × 18]
```
