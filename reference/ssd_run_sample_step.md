# Run a sample Shard

Runs the `sample` tasks bundled into one shard: under one
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope, reads each task's dataset off the scenario via
[`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md),
draws `n_max` rows with the per-task `(seed, primer)` through
`sample_data_task_primer()`, and writes one Parquet at the shard's Hive
partition path. Each task's draw is tagged with its `sample_id` and a
`.row` order index so a downstream `fit` shard can isolate and re-order
it.

## Usage

``` r
ssd_run_sample_step(tasks, scenario, out_dir)
```

## Arguments

- tasks:

  A tibble of the shard's task rows (the `tasks` list-column of a row of
  [`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_shards.md)),
  each carrying its axis values, `sample_id`, `seed`, and `primer`.

- scenario:

  The `ssdsims_scenario` (a referenced global in `_targets.R`).

- out_dir:

  The `sample` results root (e.g. `"results/sample"`).

## Value

The shard's Parquet path (the `format = "file"` contract).

## See also

[`ssd_run_fit_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_fit_step.md),
[`ssd_run_hc_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_hc_step.md),
[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_shards.md).

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
shards <- ssd_scenario_sample_shards(scenario)
dir <- tempfile()
ssd_run_sample_step(shards$tasks[[1L]], scenario, file.path(dir, "sample"))
#> [1] "/tmp/RtmpiRqPyP/file317777993c78/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
```
