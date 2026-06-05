# Run an hc Shard

Runs the `hc` tasks bundled into one shard: reads the distinct set of
parent `fit` shards the shard's tasks reference (each once - an hc shard
typically spans several fit shards), isolates each task's fit by
`fit_id`, deserialises the `fitdists` object, and estimates the hazard
concentration with the per-task `(seed, primer)` through
`hc_data_task_primer()`. Each task's hc tibble (one or more rows - the
`proportion` fan-out and the `ci = FALSE` collapse, section 1.2) is
tagged with its `hc_id` and parent `fit_id`, stacked, and written as one
Parquet at the shard's partition path.

## Usage

``` r
ssd_run_hc_step(tasks, scenario, fit_dir, out_dir)
```

## Arguments

- tasks:

  A tibble of the shard's `hc` task rows (from
  [`ssd_scenario_hc_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_shards.md)),
  each carrying its hc-grid values, `hc_id`, the parent `fit` path-axis
  values and `fit_id`, `seed`, and `primer`.

- scenario:

  The `ssdsims_scenario` (a referenced global in `_targets.R`).

- fit_dir:

  The `fit` results root the parent shards were written to.

- out_dir:

  The `hc` results root (e.g. `"results/hc"`).

## Value

The shard's Parquet path.

## Examples

``` r
# \donttest{
scenario <- ssd_define_scenario(
  ssddata::ccme_boron,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = "lnorm"
)
dir <- tempfile()
ssd_run_sample_step(
  ssd_scenario_sample_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample")
)
#> [1] "/tmp/RtmpIEdu9b/file317753b97eb7/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/RtmpIEdu9b/file317753b97eb7/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
ssd_run_hc_step(
  ssd_scenario_hc_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "fit"),
  file.path(dir, "hc")
)
#> [1] "/tmp/RtmpIEdu9b/file317753b97eb7/hc/dataset=ccme_boron/sim=1/part.parquet"
# }
```
