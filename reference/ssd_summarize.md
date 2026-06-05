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
ssd_summarize(dir_sample, dir_fit, dir_hc, path)
```

## Arguments

- dir_sample:

  The `sample` results root.

- dir_fit:

  The `fit` results root.

- dir_hc:

  The `hc` results root.

- path:

  The output Parquet path for the combined summary.

## Value

The summary Parquet path (the `format = "file"` contract).

## Details

In a `targets` pipeline a directory read carries no dependency edge, so
order `summary` after the shards by referencing an upstream barrier in
its command (see the shipped `_targets.R` template's
[`tar_combine()`](https://docs.ropensci.org/tarchetypes/reference/tar_combine.html)
barriers). Reading the directory - rather than the shard target values -
is what lets it union whatever shards landed (the survivors of a
partially-failed run, section 6.2).

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
ssd_summarize(
  file.path(run$dir, "sample"),
  file.path(run$dir, "fit"),
  file.path(run$dir, "hc"),
  file.path(run$dir, "summary.parquet")
)
#> [1] "/tmp/RtmpIEdu9b/ssdsims-shards-31772ef26c57/summary.parquet"
# }
```
