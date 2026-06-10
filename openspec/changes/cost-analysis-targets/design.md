## Context

The `cost-estimation` capability (`R/cost-estimate.R`) predicts a scenario's
compute cost from a per-`ci_method` model `time = (base + slope × max(nboot, n0))
× nrow_factor(nrow) + fixed_addend`. Its coefficients come from
`ssd_calibrate_cost()`, a synthetic micro-benchmark that resamples a reference
dataset and times tiny `ssd_hc()` calls — it never touches the real `targets`
pipeline. A real run, meanwhile, leaves a complete record of ground-truth
durations: `targets` writes a `seconds` field per target into its meta store,
readable read-only with `targets::tar_meta()`.

The pipeline (`ssd_scenario_targets()`, `R/targets-runner.R`) fans a scenario out
into one named, `format = "file"` shard target per `partition_by` path cell, via
`tarchetypes::tar_map(values = <step>_shards, names = <path axes>)`. The minted
names are `<step>_step_<pathcell>` (e.g. `hc_step_boron_1`). The shard layout —
which axes a step partitions on and which task rows fall in each cell — is a pure
function of the scenario (`ssd_scenario_<step>_shards()`,
`scenario_partition_axes()`, `path_key()`), so a target name can be mapped back
to its shard, and a shard's `tasks` list-column carries every axis value
(`ci_method`, `nboot`, `nrow`, `dataset`, `sim`, …) the cost model keys on.

This change reads that store, attributes each shard target's `seconds` to the
scenario shard (and its task axes) that produced it, and uses the result to
validate and recalibrate the estimator.

## Goals / Non-Goals

**Goals:**
- Read a completed run's per-target durations from the `targets` store and
  attribute them to scenario shards/axes (`ssd_analyse_cost()`).
- Recalibrate the existing `ssdsims_cost_calibration` model from an observed run's
  `hc`-shard durations (`ssd_calibrate_cost_from_run()`), reusing the
  `cost-estimation` fitting helpers so the object shape is identical.
- Compare predicted vs observed in one object (`ssd_compare_cost()`).
- Be strictly read-only: no pipeline execution, no RNG, no writes.

**Non-Goals:**
- Changing the cost model's *form* or any `cost-estimation` requirement (reused
  as-is).
- Profiling within a shard (per-task timing inside one target) — `targets` times
  at the target granularity, so the shard is the finest observable unit; per-task
  attribution is by even division within a shard where needed, not measured.
- Live progress monitoring of a running pipeline (this reads a *finished* store).
- Any new on-disk artifact or manifest.

## Decisions

### Attribute at the shard (target) granularity, divide to tasks only for hc fits
`targets` records one `seconds` per target, and a target is a shard (many tasks),
so the shard is the finest *measured* unit. For the totals (`total`, `longest`)
this is exact: sum and max over shard `seconds`. For the per-axis breakdown and
recalibration, an `hc` shard may bundle several tasks (different `nboot` /
`ci_method`); the cost model attributes per task by the model's own predicted
share, i.e. split a shard's observed `seconds` across its tasks in proportion to
the *predicted* per-task seconds, then aggregate by axis. This keeps the
breakdown keyed identically to `ssd_estimate_cost()` while staying honest that
the measurement is per shard. *Alternative considered:* divide equally across a
shard's tasks — rejected because `nboot` varies within a shard and equal division
would mis-rank the costly cells; proportional-to-prediction is the least-biased
split given only the shard total.

### Resolve target → shard via the scenario, not by re-parsing Hive paths
The target name suffix is produced by `tar_map`'s `names` from the path-axis
columns; the cleanest inverse is to *regenerate* the expected names from the
scenario (reuse `shard_cell_names()` / `scenario_partition_axes()` logic) and
join the store's `name` column to them, rather than string-splitting
`<step>_step_<...>` heuristically (axis values can themselves contain
separators). The scenario is the source of truth for the layout; the store
supplies only `name` and `seconds`. *Alternative considered:* parse the name with
a regex — rejected as brittle against dataset names with underscores and against
`partition_by` changes.

### Recalibration reuses `calibrate_coefficients()` / `calibrate_nrow_factor()`
`R/cost-estimate.R` already fits `time ~ pmax(nboot, n0)` per `ci_method` and the
bounded `nrow_factor` from a `sweep` data frame with columns `nrow`, `ci_method`,
`nboot`, `time`. `ssd_calibrate_cost_from_run()` builds that same `sweep` frame
from observed `hc`-shard per-task seconds and calls the existing (unexported)
helpers, so the run-derived `ssdsims_cost_calibration` is byte-shape-identical to
a sweep-derived one and drops straight into `ssd_estimate_cost()`. Only the
provenance differs (a `source = "run"` marker plus the store path and the
observed run date). *Alternative considered:* a separate fitter — rejected as
duplication; the model form is shared by contract.

### `targets` becomes a hard requirement for these functions, via `check_installed`
`targets` is in `Suggests`. The new functions `rlang::check_installed("targets")`
at entry and call `targets::tar_meta()` / `targets::tar_config_get()`; the package
still installs without `targets`, matching how `ssd_scenario_targets()` already
guards `targets`/`tarchetypes`. *Alternative considered:* promote `targets` to
`Imports` — rejected; the rest of the package (scenario, task tables, single-core
runner) needs no `targets`, and the existing factory already uses the
`check_installed` pattern.

### S3 objects mirror the cost-estimation pattern
`ssdsims_cost_analysis` and `ssdsims_cost_comparison` follow the existing
`ssdsims_cost_estimate` shape: a list with `difftime` totals, a `breakdown`
tibble, and provenance, plus `format`/`print` methods reusing `format_duration()`.
This keeps the three cost objects visually and structurally consistent.

## Risks / Trade-offs

- [Per-task attribution within a multi-task `hc` shard is inferred, not measured]
  → Document it: totals/longest are exact at shard granularity; the per-axis
  split is proportional-to-prediction. A user wanting per-task ground truth can
  push more axes into `partition_by` so each task is its own shard.
- [A target name regenerated from the scenario may not match the store if the
  scenario or `partition_by` changed since the run] → Join on name and report
  unmatched targets rather than silently dropping or aborting; surface the matched
  count so a mismatch is visible.
- [Store schema / `tar_meta()` columns could shift across `targets` versions] →
  Depend only on the long-stable `name` and `seconds` columns; guard the
  `check_installed` minimum if a floor is needed.
- [Errored shards carry `NA` seconds] → Exclude from totals (spec requirement),
  count contributors, so a partially-failed run still analyses cleanly.

## Migration Plan

Purely additive: new file `R/cost-analysis.R`, new exports, new vignette, new
`_pkgdown.yml` entries. No existing function, object, or output changes, so no
rollback beyond reverting the new file and its exports. `targets` stays a
`Suggests`; only the new functions require it at call time.

## Open Questions

- Should `ssd_compare_cost()` also emit a per-`ci_method` predicted/observed slope
  ratio (a finer recalibration diagnostic) or is the total/longest ratio enough
  for the first cut? Leaning to start with totals/longest and add the per-cell
  diagnostic if the worked vignette shows it is needed.
- Where the observed run used real parallelism, the store's `seconds` is per-target
  wall time; summing gives serial-equivalent total (consistent with
  `ssd_estimate_cost()`'s serial total). Confirm the vignette frames "total" as
  serial-equivalent to avoid confusion with elapsed wall time under workers.
