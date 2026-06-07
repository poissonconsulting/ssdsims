# Group hc Tasks into Shards

As
[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_shards.md)
for the `hc` step: groups
[`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_tasks.md)
by `partition_by$hc`. Each task row in `tasks` carries its parent `fit`
path-axis values and `fit_id`, so the runner opens the matching `fit`
shard by partition path.

## Usage

``` r
ssd_scenario_hc_shards(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

A tibble with one row per `hc` shard (path-axis columns + a `tasks`
list-column).

## See also

[`ssd_run_hc_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_hc_step.md).

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
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
