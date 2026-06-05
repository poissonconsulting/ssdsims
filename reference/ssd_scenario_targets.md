# Build the Targets Pipeline for a Scenario

A **target factory**: returns the list of `targets` objects that runs a
scenario as a static-branching Hive-sharded pipeline (TARGETS-DESIGN.md
section 6), so a whole `_targets.R` reduces to *build a scenario and
call this*:

## Usage

``` r
ssd_scenario_targets(
  scenario,
  root = scenario_results_dir(scenario),
  cue = NULL
)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- root:

  The results root the shards and summary are written under; defaults to
  the per-layout
  [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md).

- cue:

  An optional
  [`targets::tar_cue()`](https://docs.ropensci.org/targets/reference/tar_cue.html)
  applied to every shard target (e.g. `targets::tar_cue(depend = FALSE)`
  to pin trusted shards against code changes). `NULL` (default) uses
  `targets`' standard cue.

## Value

A list of `targets` target objects, for `_targets.R` to return.

## Details

    library(targets)
    library(tarchetypes)
    library(ssdsims)
    scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
    ssd_scenario_targets(scenario)

For each step it
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)s
one named, `format = "file"`, `error = "null"` target per `partition_by`
path cell (the `names` are the step's path axes), and writes every shard
and the summary under the per-layout
[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
root (so a changed `partition_by`/`bundle` never mixes shard
granularities). `scenario` is referenced as a global, so editing it
invalidates the dependent shards.

## Invalidation model

The shard targets use **content-hash invalidation over their
`format = "file"` Parquet outputs** (TARGETS-DESIGN.md section 8),
observable as **cache-by-existence**: a shard is up to date iff its
Parquet exists *and* the inputs its body depends on - its task rows, the
scenario, and the parent shard target(s) it reads - are unchanged. A
missing Parquet rebuilds; a recomputed shard whose bytes are
byte-identical leaves its dependents skipped.

Instead of a coarse `sample -> fit -> hc`
[`tar_combine()`](https://docs.ropensci.org/tarchetypes/reference/tar_combine.html)
barrier (which marks the *whole* downstream step out of date when any
one parent shard changes), each child shard target names **only the
specific parent shard target(s) its tasks read** (the Option-3 per-child
upstream edges of section 6), computed at sourcing time as
`unique(path_key(tasks, partition_by[[parent]]))` - the same projection
the runner uses to read them. So rewriting one parent shard re-runs only
the child shards that read it. `summary` reads the whole `hc` directory,
so it names every `hc` shard (it re-runs when any `hc` shard's bytes
change, and unions the survivors of a partially-failed run).

## Pinning trusted shards (`cue`)

Pass `cue = targets::tar_cue(depend = FALSE)` to **pin** the shard
targets against upstream dependency/code changes (an edited per-task
primitive, a bumped `ssdtools`), so trusted shards are not rebuilt by a
code edit (TARGETS-DESIGN.md section 8.3). The carve-outs still hold: a
shard rebuilds if its `format = "file"` Parquet is missing, if its
task-table grouping changes (the grouping is part of the command, so
path-axis and inner-axis growth still apply under the pin), or if it
previously errored. Force a refresh of chosen shards with
[`targets::tar_invalidate()`](https://docs.ropensci.org/targets/reference/tar_invalidate.html)
(or by deleting their Parquet), overriding the pin (section 8.4). The
default (`NULL`) is `targets`' standard cue.

The `head(sample, nrow)` truncation stays folded into the `fit` step (no
materialised `data` shard): a `fit` shard is keyed by `fit_id`, which
includes `nrow`, so extending `nrow` mints new `fit` shards and caches
the rest, and a widened `max(nrow)` changes the `sample` shard's `n_max`
task row, so its bytes change and the per-child edge propagates to
exactly the `fit` shards that read the wider draw - no stale short draw
is produced.

To parallelise the shards, set a controller (e.g. a mirai-backed
[`crew::crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.html))
with
[`targets::tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html)
in `_targets.R` before calling this - the target set is unchanged.

## See also

[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md),
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
(the single-core, `targets`-free equivalent).

## Examples

``` r
if (FALSE) { # \dontrun{
# _targets.R
library(targets)
library(tarchetypes)
library(ssdsims)
scenario <- ssd_define_scenario(ssddata::ccme_boron, nsim = 2L, seed = 42L)
ssd_scenario_targets(scenario)
} # }
```
