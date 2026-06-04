# Group sample Tasks into Shards

Groups the scenario's `sample` task table
([`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_tasks.md))
by its `partition_by$sample` **path** axes into one row per shard: each
shard row carries the path-axis columns (the `tar_map` target-name
suffix and Hive path) and a `tasks` list-column of that shard's task
rows, each decorated with `seed = scenario$seed` and its per-task
`primer`
([`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
over `task_axes("sample")`). The decoration is RNG-free; the bare task
table keeps its no-`(seed, primer)` contract.

## Usage

``` r
ssd_scenario_sample_shards(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

A tibble with one row per `sample` shard: the path-axis columns and a
`tasks` list-column. Suitable as `tarchetypes::tar_map(values = )`.

## See also

[`ssd_run_sample_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_sample_step.md),
[`ssd_scenario_fit_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_shards.md).

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
ssd_scenario_sample_shards(scenario)
#> # A tibble: 2 × 4
#>   dataset      sim replace              tasks
#>   <chr>      <int> <lgl>   <list<tibble[,7]>>
#> 1 ccme_boron     1 FALSE              [1 × 7]
#> 2 ccme_boron     2 FALSE              [1 × 7]
```
