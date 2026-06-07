# Run a fit Shard

Runs the `fit` tasks bundled into one shard: reads the distinct set of
parent `sample` shards the shard's tasks reference (each once - they may
span several sample shards), isolates each task's draw by `sample_id`
(restoring row order), truncates it inline (`head(sample, nrow)`,
RNG-free, section 5), and fits with the per-task `(seed, primer)`
through `fit_data_task_primer()` (resolving `min_pmix` off the scenario
via
[`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)).
The fitted `fitdists` object is serialised into a `fit_blob` string
column keyed by `fit_id`, and one Parquet is written at the shard's
partition path.

## Usage

``` r
ssd_run_fit_step(tasks, scenario, sample_dir, out_dir)
```

## Arguments

- tasks:

  A tibble of the shard's `fit` task rows (from
  [`ssd_scenario_fit_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_shards.md)),
  each carrying its fit-grid values, `fit_id`, the parent `sample`
  path-axis values, `seed`, and `primer`.

- scenario:

  The `ssdsims_scenario` (a referenced global in `_targets.R`).

- sample_dir:

  The `sample` results root the parent shards were written to.

- out_dir:

  The `fit` results root (e.g. `"results/fit"`).

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
#> [1] "/tmp/RtmpWbDMZ2/file38d0288fc9c7/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/RtmpWbDMZ2/file38d0288fc9c7/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
# }
```
