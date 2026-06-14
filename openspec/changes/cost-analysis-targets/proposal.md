## Why

The `cost-estimation` capability predicts a scenario's compute cost *before* a
run, but its per-task model is fitted from a tiny synthetic `ssd_calibrate_cost()`
sweep — never from a real scenario. A completed run leaves ground truth behind in
two places: the `targets` meta store records wall seconds per *target* (shard),
and — once the runners capture them — the shard Parquets themselves can carry
per-*task* start/end times. Today neither is read back, attributed to the
scenario axes that drove it, or checked against the prediction. This change
instruments the runners with per-task timings and closes the loop: a finished
run becomes observed per-axis cost data, both to validate (and recalibrate) the
estimator and to size the next, larger scenario from real measurements rather
than a synthetic micro-benchmark.

## What Changes

- **In-band per-task timing columns on the `fit` and `hc` shards.** Each task's
  body is bracketed in the step runners (`ssd_run_fit_step()` /
  `ssd_run_hc_step()`) and the resulting `.start` / `.end` (UTC timestamps) and
  `.host` (CPU description, the grain the architecture-specific calibration
  pools on) ride as columns of `part.parquet` — no sidecar files (a sidecar
  would double the shard-file count; at deliverable scale that is ~450k → ~900k
  objects). The `sample` step is not timed (its cost stays inside the model's
  `fixed_addend`), so sample shards keep full file-level byte-determinism.
- **`ssd_run_scenario_baseline()` carries the same columns** on its in-memory
  `fit`/`hc` tibbles, so timing capture and cost analysis work on any run —
  serial baseline, single-core `ssd_run_scenario_shards()`, or the `targets`
  pipeline. The legacy `ssd_*_sims` family is untouched (waits for
  `migrate-public-api`).
- **The byte-identity contract narrows to result columns for `fit`/`hc`.**
  Per-task *result* columns (joined on `<step>_id`) remain byte-identical to the
  baseline oracle and across re-layouts; the timing columns are run-specific by
  design and excluded. **Consequence:** a `fit`/`hc` shard's *file* hash is no
  longer deterministic across recomputes, so a forced recompute with identical
  results now re-runs its dependents and re-uploads (see design Risks; the §8.3
  pin covers the code-edit case).
- **`ssd_summarise()` keeps the hc timing columns** in both summary outputs
  (they are tiny scalars), so observed cost is queryable from `summary.parquet`
  alone — including after upload, when shards may be remote.
- Add `ssd_analyse_cost(scenario, ...)`: reads the in-band per-task timings off
  a run's `fit`/`hc` shards (or the baseline result), returning an
  `ssdsims_cost_analysis` — observed total compute, observed longest task, and
  a per-axis breakdown (`ci_method` × `nboot`) keyed like `ssd_estimate_cost()`'s.
  Given a `targets` store it additionally reads `tar_meta()` per-target seconds
  as the *shard envelope*: `target seconds − Σ task durations` = per-shard
  overhead (parent read + Parquet write + dispatch), the `partition_by`-tuning
  number. For pre-timing stores the tar_meta path is the fallback, attributing
  shard seconds to tasks proportional to the predicted per-task cost.
- Add `ssd_calibrate_cost_from_run(scenario, ...)`: re-fits the per-task cost
  model from a run's *measured* hc task durations (and a measured fit addend),
  host-aware (it does not silently pool timings from different architectures),
  returning the same `ssdsims_cost_calibration` object `ssd_calibrate_cost()`
  produces, with run-derived provenance.
- Add `ssd_compare_cost(scenario, ...)`: predicted estimate beside observed
  analysis in one object, with predicted/observed ratios for total and longest.
- Add a cost-analysis vignette demonstrating the analyse → compare → recalibrate
  loop on a small worked run.

## Capabilities

### New Capabilities
- `cost-analysis`: capture per-task start/end/host timings in the `fit`/`hc`
  shard Parquets (and the baseline runner's tibbles), read them back from a
  completed run, attribute observed cost to the scenario's axes, compare
  observed against predicted cost, and recalibrate the per-task cost model from
  a real run; the `targets` meta store supplies the per-shard envelope and the
  fallback for pre-timing runs.

### Modified Capabilities
- `shard-runner`: the byte-identity requirement ("partition_by is a free
  re-layout — per-task results are byte-identical to the in-memory baseline")
  narrows from whole-row to **result-column** identity for the `fit`/`hc` steps:
  the new `.start`/`.end`/`.host` timing columns are run-specific and excluded
  from the oracle and re-layout comparisons. `sample` shards carry no timing
  columns and keep file-level determinism.

## Impact

- New code: `R/cost-analysis.R` (analyse/compare/recalibrate, the
  `ssdsims_cost_analysis`/`ssdsims_cost_comparison` S3 objects and
  `format`/`print` methods, the tar_meta target-name resolver). Reuses
  `R/cost-estimate.R`'s `calibrate_coefficients()`/`calibrate_nrow_factor()`,
  `new_ssdsims_cost_calibration()`, and `cost_cpu_info()` (the `.host` value).
- Changed code: `R/targets-runner.R` (`ssd_run_fit_step()`/`ssd_run_hc_step()`
  bracket each task and write the timing columns; `ssd_summarise()` retains
  them), `R/run-scenario.R` (baseline runner gains the same columns on its
  `fit`/`hc` tibbles). **Run output schema changes** for `fit`/`hc` shards and
  the summaries; `sample` output is unchanged.
- Tests: the byte-identity oracle and re-layout/atomic-rewrite assertions move
  to result-column comparisons (timing columns excluded); existing snapshots of
  `fit`/`hc` shard schemas update.
- Dependencies: none added. `targets` stays in `Suggests`; only the store-reading
  paths `rlang::check_installed("targets")` at call time.
- Docs: new `cost-analysis` vignette; `_pkgdown.yml` reference entries;
  `NAMESPACE` exports for the new functions and S3 methods.
- No change to the scenario object, the task tables, task identity/primers, or
  the `sample` layer.
