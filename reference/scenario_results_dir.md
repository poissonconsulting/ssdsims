# Layout-keyed Results Root for a Scenario

Returns `<root>/layout=<hash>`, where the hash is derived from the
scenario's `partition_by`. A step's Hive shard path depth and axes are a
function of `partition_by`/`bundle`, so writing two different layouts
into one root would leave shards of *different granularity* side by
side - and the depth-agnostic glob the readers use
(`<step>/**/part.parquet`) would then union stale and current shards,
double-counting tasks. Keying the results root on the layout isolates
each `partition_by` into its own subtree: re-running a scenario with a
changed `partition_by`/`bundle` writes to a *fresh* root (never mixing
granularities), while re-running the *same* layout reuses the root
(idempotent and cache-friendly - the same shard paths are simply
rewritten).

## Usage

``` r
scenario_results_dir(scenario, root = "results")
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- root:

  The results root directory (default `"results"`).

## Value

The layout-keyed path `file.path(root, paste0("layout=", <hash>))`.

## Details

The `targets` pipeline writes under this root (see the shipped
`_targets.R` template). The single-core
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
takes the complementary approach: it *owns* and clears a fixed `dir` on
each run.

## See also

[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md),
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md).

## Examples

``` r
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 1L, seed = 42L)
scenario_results_dir(scenario)
#> [1] "results/layout=b078bfa16655"
```
