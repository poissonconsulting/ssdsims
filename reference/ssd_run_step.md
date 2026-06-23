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
  draw the effective draw size - the scenario's `nrow_max` setting,
  capped at the dataset size for `replace = FALSE` - through
  `sample_data_task_primer()`, and tag each draw with its `sample_id`
  and a `.row` order index so a downstream `fit` shard can isolate and
  re-order it.

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
  shard typically spans several fit shards), decode each parent
  **union** fit once per `fit_id` (reused across every `distset` task
  that shares it), resolve each task's `distset` name to its members via
  [`scenario_distset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_distset.md),
  subset the union fit to that pool (`strict = FALSE`), and estimate the
  hazard concentration with the per-task `(seed, primer)` through
  `hc_data_task_primer()` (the subset happens in that shared primitive).
  Each task's hc tibble (with the scalar `ci` applied uniformly and
  bootstrap-only scenario options `NA` when `ci = FALSE`) is tagged with
  its `hc_id`, parent `fit_id`, and `distset` name, stacked, and written
  as one Parquet at the shard's partition path. A set whose members all
  dropped from the union fit emits no rows for that cell (the survivor
  model).

  The four non-axis hc readout settings (`proportion`, `est_method`,
  `ci`, `samples`) default to the scenario slice, so the single-scenario
  and standalone paths are byte-identical. When the shard's `tasks`
  carry per-task `proportion`/`est_method`/`ci`/`samples` columns (the
  design factory's per-overlap aggregated demand,
  [`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md)),
  each task is summarised with its own cell's demand instead - the
  maximal readout set the per-member summary then filters.

## See also

[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
(the shard grouping these consume),
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md),
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 1L, seed = 42L)
shards <- ssd_scenario_sample_shards(scenario)
dir <- tempfile()
ssd_run_sample_step(shards$tasks[[1L]], scenario, file.path(dir, "sample"))
#> [1] "/tmp/Rtmp4RDcVE/file1b081d21b376/sample/dataset=ccme_boron/sim=1/replace=TRUE/part.parquet"
# \donttest{
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = ssd_distset(lnorm = "lnorm")
)
dir <- tempfile()
ssd_run_sample_step(
  ssd_scenario_sample_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample")
)
#> [1] "/tmp/Rtmp4RDcVE/file1b0853a005c1/sample/dataset=ccme_boron/sim=1/replace=TRUE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/Rtmp4RDcVE/file1b0853a005c1/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
# }
# \donttest{
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = ssd_distset(lnorm = "lnorm")
)
dir <- tempfile()
ssd_run_sample_step(
  ssd_scenario_sample_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample")
)
#> [1] "/tmp/Rtmp4RDcVE/file1b08bf4d5b2/sample/dataset=ccme_boron/sim=1/replace=TRUE/part.parquet"
ssd_run_fit_step(
  ssd_scenario_fit_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "sample"),
  file.path(dir, "fit")
)
#> [1] "/tmp/Rtmp4RDcVE/file1b08bf4d5b2/fit/dataset=ccme_boron/sim=1/nrow=6/rescale=FALSE/part.parquet"
ssd_run_hc_step(
  ssd_scenario_hc_shards(scenario)$tasks[[1L]],
  scenario,
  file.path(dir, "fit"),
  file.path(dir, "hc")
)
#> [1] "/tmp/Rtmp4RDcVE/file1b08bf4d5b2/hc/dataset=ccme_boron/sim=1/part.parquet"
# }
```
