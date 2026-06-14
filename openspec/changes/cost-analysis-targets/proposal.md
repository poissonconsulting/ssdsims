## Why

The `cost-estimation` capability predicts a scenario's compute cost *before* a
run, but its per-task model is only ever fitted from a tiny synthetic
`ssd_calibrate_cost()` sweep — never from a real scenario. A completed run is the
best calibration data there is, but the runners discard the per-task durations
that would make it usable. This change captures those durations in the run's
output and adds **one** new entry point — calibrate the existing cost model from
a finished run's result summary — so the path *result summary → estimation*
closes through the machinery that already exists (`ssd_estimate_cost()`,
`ssdsims_cost_calibration`).

Everything that is *analysis of a data frame* — totalling observed durations,
finding the longest task, comparing observed against predicted — is left to
**documentation** (a vignette with `dplyr` recipes), not new code. The earlier
full-fat proposal (#152) added two bespoke S3 objects (`ssdsims_cost_analysis`,
`ssdsims_cost_comparison`) and three functions; this revision descopes them. The
observed-cost "object" was essentially *the summary minus samples* — a tibble the
run already produces — so it earned no separate type, and predicted-vs-observed
comparison is a join, not an API.

## What Changes

- **In-band per-task timing columns on the `fit` and `hc` shards.** Each task's
  body is bracketed in the step runners (`ssd_run_fit_step()` /
  `ssd_run_hc_step()`) and the resulting `.start` / `.end` (UTC timestamps) and
  `.host` (CPU description — the grain the architecture-specific calibration
  pools on, and what makes a run self-describing for calibration provenance)
  ride as columns of `part.parquet`. No sidecar files — a sidecar would double
  the shard-file count (at deliverable scale ~450k → ~900k objects). The
  `sample` step is **not** timed (its cost stays inside the model's
  `fixed_addend`), so sample shards keep full file-level byte-determinism.
- **`ssd_run_scenario_baseline()` carries the same columns** on its in-memory
  `fit`/`hc` tibbles, so a run is calibratable whether it came from the serial
  baseline, single-core `ssd_run_scenario_shards()`, or the `targets` pipeline.
  The legacy `ssd_*_sims` family is untouched (waits for `migrate-public-api`).
- **`ssd_summarise()` retains the `fit`/`hc` timing columns** in both summary
  outputs (tiny scalar columns), so the run summary alone is sufficient input to
  calibration — including after upload, when the shards may be remote.
- **The byte-identity contract narrows to result columns for `fit`/`hc`.**
  Per-task *result* columns (joined on `<step>_id`) stay byte-identical to the
  baseline oracle and across re-layouts; the timing columns are run-specific by
  design and excluded. **Consequence:** a `fit`/`hc` shard's *file* hash is no
  longer deterministic across recomputes, so a forced recompute with identical
  results re-runs its dependents and re-uploads (design Risks; the §8.3 pin
  covers the code-edit case).
- **One new function: `ssd_calibrate_cost_from_run(summary, ...)`.** It fits the
  *existing* per-task cost model from a run summary's measured `hc` durations
  (and derives `fixed_addend` from the measured `fit` durations), reusing the
  internal `calibrate_coefficients()` / `calibrate_nrow_factor()` fitters, and
  returns an `ssdsims_cost_calibration` of the same shape `ssd_calibrate_cost()`
  produces — with run-derived provenance read from `.host`. It is host-aware: a
  summary spanning more than one `.host` requires an explicit choice rather than
  silently pooling architectures. The result feeds the unchanged
  `ssd_estimate_cost(scenario, calibration)`.
- **A vignette** demonstrates the loop as documentation: read the summary, total
  and rank observed durations with `dplyr`, calibrate from it, re-estimate, and
  place predicted beside observed. No `ssd_analyse_cost()` / `ssd_compare_cost()`
  functions; no `ssdsims_cost_analysis` / `ssdsims_cost_comparison` objects.

## Capabilities

### New Capabilities
<!-- None. The new function extends cost-estimation; the instrumentation extends
     shard-runner. No bespoke cost-analysis capability or objects are introduced. -->

### Modified Capabilities
- `cost-estimation`: add a second calibration source — `ssd_calibrate_cost_from_run()`
  fits the existing model from a real run's result summary (not only the synthetic
  `ssd_calibrate_cost()` sweep), returning the same `ssdsims_cost_calibration` the
  estimator already consumes; the documentation vignette gains the
  summary → calibrate → estimate → compare loop (comparison as a `dplyr` recipe).
- `shard-runner`: the `fit`/`hc` shards (and the baseline runner's tibbles, and
  the retained summaries) carry per-task `.start`/`.end`/`.host` timing columns;
  the byte-identity requirement narrows from whole-row to **result-column**
  identity for `fit`/`hc` (timing columns excluded). `sample` shards carry no
  timing columns and keep file-level determinism.

## Impact

- New code: `R/cost-estimate.R` gains `ssd_calibrate_cost_from_run()` (reusing the
  existing `calibrate_coefficients()` / `calibrate_nrow_factor()` /
  `new_ssdsims_cost_calibration()` / `cost_cpu_info()` internals). No new S3
  classes, no new file.
- Changed code: `R/targets-runner.R` (`ssd_run_fit_step()`/`ssd_run_hc_step()`
  bracket each task and write the timing columns; `ssd_summarise()` retains
  them), `R/run-scenario.R` (baseline runner gains the same columns on its
  `fit`/`hc` tibbles). **Run output schema changes** for `fit`/`hc` shards and
  the summaries; `sample` output is unchanged.
- Tests: the byte-identity oracle and re-layout/atomic-rewrite assertions move to
  result-column comparisons (timing columns excluded); `fit`/`hc` shard-schema
  snapshots update.
- Dependencies: none added. No `targets` dependency on the calibration path
  (it reads the summary the user already has).
- Docs: the existing cost-estimation vignette (or a sibling) gains the
  summary → calibrate → estimate → compare loop; `_pkgdown.yml` reference entry
  for the new function; `NAMESPACE` export for it.
- No change to the scenario object, the task tables, task identity/primers, or
  the `sample` layer. No bespoke analysis/comparison objects.
