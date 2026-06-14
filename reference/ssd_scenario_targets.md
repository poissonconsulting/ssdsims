# Build the Targets Pipeline for a Scenario

A **target factory**: returns the list of `targets` objects that runs a
scenario as a static-branching Hive-sharded pipeline (TARGETS-DESIGN.md
section 6), so a whole `_targets.R` reduces to *build a scenario and
call this*:

## Usage

``` r
ssd_scenario_targets(
  scenario,
  ...,
  root = scenario_results_dir(scenario),
  upload = NULL,
  cue = NULL
)
```

## Arguments

- scenario:

  An `ssdsims_scenario` from
  [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md).

- ...:

  Unused; must be empty. Its presence forces `root`, `upload`, and `cue`
  to be passed **by name**
  ([`rlang::check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.html)
  aborts on a positional or misspelled argument), since `root` and
  `upload` are both path-shaped and easy to transpose.

- root:

  The results root the shards and summary are written under; defaults to
  the per-layout
  [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md).

- upload:

  An optional upload destination (the remote-destination sibling of
  `root`) from
  [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  or
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md),
  or `NULL` (default) for no upload targets. See the section above.

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
    data <- ssd_scenario_data(ssddata::ccme_boron)
    scenario <- ssd_define_scenario(data, nsim = 2L, seed = 42L)
    ssd_scenario_targets(scenario)

The shard and summary targets carry `error = "null"` so a shard whose
body fails entirely goes `NULL` (its error readable via
[`tar_meta()`](https://docs.ropensci.org/targets/reference/tar_meta.html))
without aborting the run, and
[`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md)
unions whatever landed (TARGETS-DESIGN.md section 6.2). The shipped
`_targets.R` templates pair this with a pipeline-wide **keep-going**
default (`tar_option_set(error = "continue")`, the `make -k` analogue)
so an errored target skips only its dependents while the rest of the
shards still build; fail-fast pre-flight checks (upload/cluster
connectivity) belong in a separate script the user runs *before*
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
not in this DAG.

For each step it
[`tarchetypes::tar_map()`](https://docs.ropensci.org/tarchetypes/reference/tar_map.html)s
one named, `format = "file"`, `error = "null"` target per `partition_by`
path cell (the `names` are the step's path axes), and writes every shard
and the summary under the per-layout
[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
root (so a changed `partition_by`/`bundle` never mixes shard
granularities). Each step's command depends only on the **minimal
scenario slice** its runner consumes (`scenario_step_slice()`) rather
than the bare `scenario` global, so editing a field a step does not read
leaves the other steps' shards cached. The `sample` slice is built **per
shard**, carrying only the dataset(s) that shard reads, so appending a
dataset mints a new shard and leaves every existing shard cached.

## Invalidation model

The shard targets use **content-hash invalidation over their
`format = "file"` Parquet outputs** (TARGETS-DESIGN.md section 8),
observable as **cache-by-existence**: a shard is up to date iff its
Parquet exists *and* the inputs its body depends on - its task rows, the
step's minimal scenario slice (`scenario_step_slice()`), and the parent
shard target(s) it reads - are unchanged. A missing Parquet rebuilds; a
recomputed shard whose bytes are byte-identical leaves its dependents
skipped.

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
the rest. The shared draw is sized by the scenario's fixed `nrow_max`
setting (carried on the `sample` slice), not `max(nrow)`, so extending
`nrow` within the effective draw size leaves the `sample` shards cached
too; changing `nrow_max` invalidates the `sample` slice and rebuilds the
draw, propagating through the per-child edges - no stale short draw can
arise.

To parallelise the shards, set a controller (e.g. a mirai-backed
[`crew::crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.html))
with
[`targets::tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html)
in `_targets.R` before calling this - the target set is unchanged.

## Uploading shards to cloud storage (`upload`)

`upload` is the **remote-destination sibling of `root`** (default
`NULL`). With `upload = NULL` the pipeline contains **no**
`upload_<step>` targets - the clean default DAG for a non-uploader. With
a non-`NULL` upload object the factory pairs each step shard with an
`upload_<step>` target in the same `tar_map` (`format = "file"`,
`error = "null"`), so an unchanged shard is never re-uploaded
(content-hash skip) and a per-shard upload failure isolates to its own
branch. Pass
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
for no-op upload targets that reach no network (exercising the DAG shape
offline / in CI) or
[`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
to ship to Azure. The factory performs **no** network I/O and never runs
the
[`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
probe: it only assembles the target list, so sourcing `_targets.R`
(which `targets` does on every
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html),
[`tar_manifest()`](https://docs.ropensci.org/targets/reference/tar_manifest.html),
[`tar_visnetwork()`](https://docs.ropensci.org/targets/reference/tar_visnetwork.html),
and on each worker) stays side-effect free. Run
`ssd_test_upload(upload)` yourself as a one-line preflight before
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
to confirm credentials and connectivity up front; a missing credential
still fails loud per-shard at upload time as a backstop. The per-task
results are byte-identical across all three `upload` modes; only the
presence and behaviour of the `upload_<step>` targets differ.

## See also

[`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md),
[`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
(the single-core, `targets`-free equivalent),
[`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# _targets.R
library(targets)
library(tarchetypes)
library(ssdsims)
data <- ssd_scenario_data(ssddata::ccme_boron)
scenario <- ssd_define_scenario(data, nsim = 2L, seed = 42L)
ssd_scenario_targets(scenario)

# Pair each shard with a (no-op) upload target, exercised offline:
ssd_scenario_targets(scenario, upload = ssd_upload_dryrun())
} # }
```
