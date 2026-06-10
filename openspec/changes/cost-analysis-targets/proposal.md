## Why

The `cost-estimation` capability predicts a scenario's compute cost *before* a
run, but its per-task model is fitted from a tiny synthetic `ssd_calibrate_cost()`
sweep — never from a real scenario executed through the `targets` pipeline. A
completed run already records the ground truth (`targets` stores a `seconds`
field per target in its meta store), yet the project has no way to read those
durations back, attribute them to the scenario axes that drove them, or check
the prediction against what actually happened. This change closes that loop:
it turns a finished run into observed per-axis cost data, both to validate (and
recalibrate) the estimator and to size the *next, larger* scenario from real
measurements rather than a synthetic micro-benchmark.

## What Changes

- Add `ssd_analyse_cost(scenario, store = targets::tar_config_get("store"))`:
  reads the run's `targets` meta store, pulls the per-target `seconds`, and maps
  each shard target back to the scenario shard (and its tasks) that produced it,
  returning an `ssdsims_cost_analysis` object — observed total compute, observed
  longest task/shard, and a per-axis breakdown (`ci_method` × `nboot`, plus
  `dataset`/`sim`/`nrow`) keyed the same way `ssd_estimate_cost()`'s breakdown is.
- Add an internal target-name → shard resolver that parses a `tar_map`-minted
  shard target name (`<step>_step_<pathcell>`) back to its `partition_by` path
  cell, so observed durations attribute to the right step and axes without
  re-running anything. Read-only over the store; the scenario supplies the shard
  layout.
- Add `ssd_calibrate_cost_from_run(scenario, store = ...)`: re-fit the per-task
  cost model from an observed run's hc-shard durations, returning the same
  `ssdsims_cost_calibration` object `ssd_calibrate_cost()` produces (provenance
  marked as run-derived), so a real scenario — not the synthetic sweep — can
  become the estimator's coefficients.
- Add `ssd_compare_cost(scenario, store = ...)`: place the predicted estimate
  (`ssd_estimate_cost()`) beside the observed analysis (`ssd_analyse_cost()`) in
  one object, reporting the ratio of predicted to observed total and longest, for
  validating and tuning the model.
- Add a cost-analysis vignette demonstrating the analyse → compare → recalibrate
  loop on a small worked run.

## Capabilities

### New Capabilities
- `cost-analysis`: read a completed `targets` run's per-target durations from the
  store, attribute them back to the scenario's shards and axes, compare observed
  against predicted cost, and recalibrate the per-task cost model from a real run.

### Modified Capabilities
<!-- The `cost-estimation` predictor and its calibration object are reused, not
     respecified: cost-analysis produces an `ssdsims_cost_calibration` of the same
     shape and reads `ssd_estimate_cost()` for comparison, but no cost-estimation
     requirement changes. Left empty intentionally. -->

## Impact

- New code: `R/cost-analysis.R` (the analyse/compare/recalibrate functions, the
  `ssdsims_cost_analysis`/`ssdsims_cost_comparison` S3 objects and their
  `format`/`print` methods, the target-name resolver). Reuses
  `R/cost-estimate.R`'s `calibrate_coefficients()`/`calibrate_nrow_factor()` and
  `new_ssdsims_cost_calibration()`, and the shard machinery in `R/task-shards.R` /
  `R/targets-runner.R` (`scenario_partition_axes()`, `shard_cell_names()` naming).
- Dependencies: `targets` moves to a hard requirement for these functions
  (already a `Suggests`); the functions `rlang::check_installed("targets")` and
  read the store via `targets::tar_meta()`, performing no pipeline execution.
- Docs: new `cost-analysis` vignette; `_pkgdown.yml` reference index; `NAMESPACE`
  exports for the new functions and S3 methods.
- No change to the scenario object, the task tables, the shard runners, or any
  run output — the capability is strictly read-only over an existing store.
