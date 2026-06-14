# Summarise a Run's hc Estimates Across Shards

Fans in the run's results without pulling shard target values back into
R or recomputing anything: reads every `hc` shard Parquet under `dir_hc`
(a Hive glob) with `duckplyr` - the analysis-ready per-task
hazard-concentration estimates - unions them, and writes `path`. Because
it reads the result directory (not the shard targets), it sees whatever
shards landed, so it unions the survivors of a partially-failed run
(`error = "null"`, section 6.2). `dir_sample` and `dir_fit` are accepted
for signature symmetry with the three result layers; the `sample` draws
and serialised `fit` objects are not summary material, so the combined
summary is the `hc` layer.

## Usage

``` r
ssd_summarise(
  dir_sample,
  dir_fit,
  dir_hc,
  path,
  path_with_samples = NULL,
  samples_row_group_bytes = "100MB"
)
```

## Arguments

- dir_sample:

  The `sample` results root.

- dir_fit:

  The `fit` results root.

- dir_hc:

  The `hc` results root.

- path:

  The output Parquet path for the compact summary (`dists`/`samples`
  projected out).

- path_with_samples:

  Optional output Parquet path for a full summary that retains the
  `dists`/`samples` list-columns. `NULL` (the default) writes only the
  compact summary.

- samples_row_group_bytes:

  The Parquet row-group byte budget for the `path_with_samples` write (a
  string DuckDB's `ROW_GROUP_SIZE_BYTES` accepts, default `"100MB"`);
  see the *Memory and the full summary's row groups* section. Ignored
  when `path_with_samples` is `NULL`.

## Value

The summary Parquet path(s) (the `format = "file"` contract): `path`
when `path_with_samples` is `NULL`, otherwise
`c(path, path_with_samples)`.

## Details

The compact summary at `path` projects the `dists`/`samples`
list-columns out at the DuckDB level, so the potentially-large retained
bootstrap draws are never pulled into R. Supply `path_with_samples` to
**also** write a full summary that retains those list-columns: that
write reuses the same lazy DuckDB read, so the draws never materialise
in R there either. The draws are populated only when the scenario set
`samples = TRUE`, so the full summary is the analysis-ready estimates
plus the per-row draws.

In a `targets` pipeline a directory read carries no dependency edge, so
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
orders `summary` after the shards by naming every `hc` shard target in
its command (it re-runs when any `hc` shard's bytes change). Reading the
directory - rather than the shard target values - is what lets it union
whatever shards landed (the survivors of a partially-failed run, section
6.2).

## Memory and the full summary's row groups

The full summary is written in **byte-budgeted Parquet row groups**
(`samples_row_group_bytes`, default `"100MB"`), so its memory
requirement follows the per-group budget - about five times the budget -
rather than the union's total row count, and the row-group row count
adapts to the `samples` cell size (large groups for small draws, small
groups for large ones). The engine accepts the byte budget because the
pipeline configuration scope holds `preserve_insertion_order = false`
(restored when `ssd_summarise()` returns); it is refused while
preserving order, and only the global setting counts - the per-copy
`PRESERVE_ORDER` option cannot substitute. The trade: the full summary's
**row order is not contractual** - re-summarising the same shards yields
the same rows (address them by `hc_id`/`fit_id`), but their order and
the file's bytes may differ. Under the default single thread, writes
were observed in input order and byte-identical across runs regardless.
Evidence: the `duckplyr-config` change's
`exploration/experiment-summary-union.R`,
`exploration/experiment-rgbytes.R`, and
`exploration/experiment-preserve-order-copy-option.R`.

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
# Materialise the shards single-core, then fan in the hc layer.
run <- ssd_run_scenario_shards(scenario)
ssd_summarise(
  file.path(run$dir, "sample"),
  file.path(run$dir, "fit"),
  file.path(run$dir, "hc"),
  file.path(run$dir, "summary.parquet")
)
#> [1] "/tmp/RtmpvBEnv9/ssdsims-shards-36ed6520f35f/summary.parquet"
# }
```
