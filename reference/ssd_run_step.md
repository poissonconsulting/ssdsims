# Run a Step Shard

The per-shard step runners the `targets` pipeline (and the single-core
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md))
call - one target per shard, one runner per step. Each takes a shard's
`tasks` (the `tasks` list-column of a row of the matching
[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
family), runs the bundled tasks with the *same* per-task seed-and-run
primitives the baseline runner uses (`*_data_task_primer()`) under one
[`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
scope, reads any upstream shard back from Parquet by partition path, and
writes one Parquet at the shard's Hive partition path - returning that
path (the `format = "file"` contract). Because a task's result is fully
determined by its `(seed, primer)` and is order-independent, the
per-task results are byte-identical to
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
regardless of how tasks bundle into shards.

## Usage

``` r
ssd_run_sample_step(tasks, scenario, out_dir)

ssd_run_fit_step(tasks, scenario, sample_dir, out_dir)

ssd_run_hc_step(tasks, scenario, fit_dir, out_dir)
```

## Arguments

- tasks:

  A tibble of the shard's task rows (the `tasks` list-column of a row of
  the matching `ssd_scenario_*_shards()`), each carrying the step's axis
  values, its `<step>_id` key, `seed`, and `primer` - and, for
  `fit`/`hc`, the parent step's path-axis values and `<parent>_id`.

- scenario:

  The `ssdsims_scenario` (a referenced global in `_targets.R`).

- out_dir:

  The step's results root (e.g. `"results/sample"`).

- sample_dir:

  The `sample` results root the parent shards were written to (the `fit`
  step).

- fit_dir:

  The `fit` results root the parent shards were written to (the `hc`
  step).

## Value

The shard's Parquet path (the `format = "file"` contract).

## Functions

- `ssd_run_sample_step()`: Run the `sample` tasks: read each task's
  dataset off the scenario via
  [`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md),
  draw `n_max` rows through `sample_data_task_primer()`, and tag each
  draw with its `sample_id` and a `.row` order index so a downstream
  `fit` shard can isolate and re-order it.

- `ssd_run_fit_step()`: Run the `fit` tasks: read the distinct set of
  parent `sample` shards the shard's tasks reference (each once - they
  may span several sample shards), isolate each task's draw by
  `sample_id` (restoring row order), truncate it inline
  (`head(sample, nrow)`, RNG-free, section 5), and fit with the per-task
  `(seed, primer)` through `fit_data_task_primer()` (resolving
  `min_pmix` off the scenario via
  [`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)).
  The fitted `fitdists` object is serialised into a `fit_blob` string
  column keyed by `fit_id`, and one Parquet is written at the shard's
  partition path.

- `ssd_run_hc_step()`: Run the `hc` tasks: read the distinct set of
  parent `fit` shards the shard's tasks reference (each once - an hc
  shard typically spans several fit shards), isolate each task's fit by
  `fit_id`, deserialise the `fitdists` object, and estimate the hazard
  concentration with the per-task `(seed, primer)` through
  `hc_data_task_primer()`. Each task's hc tibble (one or more rows - the
  `proportion` fan-out, with the scalar `ci` applied uniformly and
  bootstrap-only knobs `NA` when `ci = FALSE`) is tagged with its
  `hc_id` and parent `fit_id`, stacked, and written as one Parquet at
  the shard's partition path.

## See also

[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
(the shard grouping these consume),
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md),
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md).

## Examples

``` r
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 1L, seed = 42L)
shards <- ssd_scenario_sample_shards(scenario)
dir <- tempfile()
ssd_run_sample_step(shards$tasks[[1L]], scenario, file.path(dir, "sample"))
#> [1] "/tmp/RtmpKFRZ4d/file36816f3d59e6/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
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
#> [1] "/tmp/RtmpKFRZ4d/file36818c655bf/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/RtmpKFRZ4d/file36818c655bf/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
# }
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
#> [1] "/tmp/RtmpKFRZ4d/file368163326269/sample/dataset=ccme_boron/sim=1/replace=FALSE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/RtmpKFRZ4d/file368163326269/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
ssd_run_hc_step(
  ssd_scenario_hc_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "fit"),
  file.path(dir, "hc")
)
#> [1] "/tmp/RtmpKFRZ4d/file368163326269/hc/dataset=ccme_boron/sim=1/part.parquet"
# }
```
