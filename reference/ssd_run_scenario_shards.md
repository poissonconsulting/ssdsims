# Run a Scenario over Hive-partitioned Parquet Shards (single core)

Executes a scenario's three task steps in dependency order - `sample`,
then `fit`, then `hc` - materialising each step's results as **one
Parquet per `partition_by` path cell** under a Hive-partitioned tree
`<dir>/<step>/<axis=value>/.../part.parquet`, and linking steps by
reading the parent step's shards back via `duckplyr` (predicate
pushdown), rather than threading results in memory. This is the
single-core, `targets`-free sibling of
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
and the first consumer of `partition-by`'s path/inner split
([`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md)'s
sibling `scenario_partition_axes()`).

## Usage

``` r
ssd_run_scenario_shards(scenario, dir = tempfile("ssdsims-shards-"))
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- dir:

  A results root to write the Hive-partitioned shards under; created if
  absent. Defaults to a per-run session temp dir (the shards are left on
  disk for inspection and reuse). The runner **owns** the
  `sample`/`fit`/`hc` subtrees under `dir` and clears them on each run,
  so replaying a scenario with a changed `partition_by`/`bundle` never
  leaves stale-granularity shards beside the new ones. (The `targets`
  pipeline instead isolates each layout under its own
  [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
  root.)

## Value

An `ssdsims_shard_run` object: a list with `dir` and the written
`sample`, `fit`, and `hc` shard Parquet paths (one per shard).

## Details

It reuses the per-task seed-and-run wrappers, so for a fixed
`scenario$seed` it is reproducible and **order-independent**, and its
per-task results are **byte-identical** to
[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md) -
`partition_by` is a free re-layout that moves only file paths, never
results. The **m:n** parent-shard dependency (a child shard reading
several parent shards, or a parent shard feeding several children, per
the section 5 coarsening defaults) is resolved at read time: each task
opens the parent shard at its `<parent>_id` identity projected onto the
parent's path axes and filters to the rows it needs.

No `targets`, `crew`, manifest, or cloud upload - this is the plain-R
storage loop only, de-risking `hive-partitioning`/`task-tables`.

## See also

[`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
(the in-memory reference oracle),
[`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md),
[`ssd_run_sample_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_step.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(
  data,
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = ssd_distset(lnorm = "lnorm")
)
run <- ssd_run_scenario_shards(scenario)
run$hc
#> [1] "/tmp/Rtmp87x0vc/ssdsims-shards-3c4627e2afbe/hc/dataset=ccme_boron/sim=1/part.parquet"
```
