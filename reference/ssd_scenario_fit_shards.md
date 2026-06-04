# Group fit Tasks into Shards

As
[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_shards.md)
for the `fit` step: groups
[`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_tasks.md)
by `partition_by$fit`. Each task row in `tasks` carries its parent
`sample` path-axis values and `sample_id`, so the runner opens the
matching `sample` shard by partition path.

## Usage

``` r
ssd_scenario_fit_shards(scenario)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

## Value

A tibble with one row per `fit` shard (path-axis columns + a `tasks`
list-column).

## See also

[`ssd_run_fit_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_fit_step.md).

## Examples

``` r
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
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
```
