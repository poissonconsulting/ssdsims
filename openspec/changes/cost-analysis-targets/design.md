## Context

The `cost-estimation` capability (`R/cost-estimate.R`) predicts a scenario's
compute cost from a per-`ci_method` model `time = (base + slope × max(nboot, n0))
× nrow_factor(nrow) + fixed_addend`, calibrated by a synthetic micro-benchmark
(`ssd_calibrate_cost()`) that never touches the real pipeline. A real run leaves
ground truth behind at two granularities:

- the `targets` meta store records wall `seconds` per *target* — i.e. per
  **shard**, since `ssd_scenario_targets()` mints one named `format = "file"`
  target per `partition_by` path cell (`<step>_step_<pathcell>`);
- the *task* granularity does not exist anywhere today — an `hc` shard bundles
  tasks with different `nboot`/`ci_method`, so shard seconds alone cannot be
  attributed to the model's axes without inference.

This change instruments the `fit`/`hc` runners with per-task timings carried
**in the shard Parquets themselves**, and builds the analysis on top: measured
per-task durations as the primary source, the targets store as the per-shard
envelope and as the fallback for pre-timing runs.

Constraints that shaped the design: deliverable-scale scenarios run to the order
of ~450k shard files, so the shard-file count is the object budget; the
shard-runner spec pins per-task results byte-identical to the in-memory baseline
oracle; the invalidation and upload models content-hash the `format = "file"`
Parquets; the per-scenario manifest is parked (no sidecar provenance mechanism
exists or should be revived here).

## Goals / Non-Goals

**Goals:**
- Measure per-task cost in-band: `.start`/`.end`/`.host` columns on the `fit`
  and `hc` shard rows, captured by the step runners and the baseline runner.
- Read a run's observed cost back and attribute it to scenario axes
  (`ssd_analyse_cost()`); compare against the prediction (`ssd_compare_cost()`);
  recalibrate the model from measured durations (`ssd_calibrate_cost_from_run()`).
- Use the targets store for what only it can say: the per-shard envelope
  (`target seconds − Σ task durations` = read/write/dispatch overhead, the
  `partition_by`-tuning number) and the fallback for pre-timing runs.
- Keep the analysis functions strictly read-only (no pipeline, no RNG, no writes).

**Non-Goals:**
- Changing the cost model's *form* or any `cost-estimation` requirement.
- Sidecar files or any per-shard provenance artifact (no manifest revival): the
  timing data rides inside `part.parquet`, adding zero files.
- Timing the `sample` step (cheap; stays inside the model's `fixed_addend` —
  and keeps the sample layer file-level deterministic).
- Instrumenting the legacy `ssd_*_sims` family (waits for `migrate-public-api`).
- Live progress monitoring of a running pipeline.

## Decisions

### Timing columns in-band, not sidecars
A per-shard `timings.parquet` sidecar would keep `part.parquet` byte-stable but
**doubles the object count** (~450k → ~900k files at deliverable scale): inode
pressure, blob-store object counts, and every glob/listing operation pay for a
few bytes of timing per task. In-band columns add zero files, survive
`shard-failure-survival`'s future shorter-shard semantics for free (survivors'
timings are just surviving rows), and travel with uploads. *Rejected
alternatives:* sidecar files (file count); the targets store alone (shard
granularity only, targets-only, prunable, doesn't travel).

### Byte-identity narrows to result columns — fit/hc only
The shard-runner contract ("per-task results byte-identical to the baseline
oracle") is restated over **result columns** joined on `<step>_id`, with
`.start`/`.end`/`.host` enumerated out (a delta on the `shard-runner` spec).
`sample` is not timed, so sample shards keep full file-level determinism. The
real cost is that a `fit`/`hc` shard's **file hash is no longer deterministic
across recomputes**: a forced recompute with identical results now re-runs
dependents and re-uploads (see Risks). Traced against the §8 extension stories
this loss is narrow — inner-axis growth changes result bytes anyway, and the
code-edit-without-result-change case is what the §8.3 pin
(`tar_cue(depend = FALSE)`) is designed for.

### `.start`/`.end` as UTC timestamps, `.host` as the CPU description
Start+end (not duration-only) costs the same storage and is strictly richer: it
reconstructs the run's actual concurrency (per-worker Gantt, straggler
detection, whether the longest task gated wall time). Stored as Parquet
TIMESTAMP (UTC) via duckplyr; duration is derived. `.host` carries the existing
`cost_cpu_info()` description — the grain the architecture-specific calibration
pools on (identical cluster nodes pool together; a nodename would fragment
them). On the `hc` layer the values repeat across a task's `proportion` rows and
RLE-compress to ~nothing.

### Capture in the runners, not the task primitives
The `*_data_task_primer()` primitives return domain objects (a `fitdists`, an hc
tibble); bracketing each task in the step-runner loops (`ssd_run_fit_step()`,
`ssd_run_hc_step()`, and the baseline's fit/hc loops) keeps the primitives pure
and the capture in one obvious place per runner. `Sys.time()` is RNG-neutral and
its microsecond resolution is ample against `fixed_addend`-scale (~0.05 s) tasks.

### Summaries keep the timing columns
`ssd_summarise()` continues to project out `dists`/`samples` but retains
`.start`/`.end`/`.host`: tiny scalar columns that make observed hc cost
queryable from `summary.parquet` alone — no 450k-file glob, and it works after
upload when shards are remote. `ssd_analyse_cost()` itself reads the shard glob
(it needs the `fit` layer too, and projects only id + timing columns at the
DuckDB level, never decoding blobs); the summary is the convenient ad-hoc query
surface, not the function's input.

### Targets store: envelope and fallback, resolved via the scenario
With measured task durations in-band, `tar_meta()$seconds` is demoted to what
only it can say: the per-shard envelope (`target seconds − Σ task durations`),
and the fallback for runs predating the timing columns (attribution proportional
to the *predicted* per-task cost, marked as inferred). Target names resolve back
to shards by **regenerating** the expected `<step>_step_<pathcell>` names from
the scenario (reusing the `shard_cell_names()`/`scenario_partition_axes()`
logic) and joining on the store's `name` column — never by string-parsing the
name (axis values can contain separators). Unmatched targets are reported, not
silently dropped; `NA`-seconds (errored/unbuilt) targets are excluded from
totals with the contributing count surfaced.

### Recalibration reuses the existing fitters, host-aware
`ssd_calibrate_cost_from_run()` builds the same sweep frame
(`nrow`, `ci_method`, `nboot`, `time`) `ssd_calibrate_cost()` fits — now from
measured hc task durations — and calls the existing unexported
`calibrate_coefficients()`/`calibrate_nrow_factor()`, so the run-derived
`ssdsims_cost_calibration` is shape-identical and drops straight into
`ssd_estimate_cost()`. The fixed addend comes from measured fit durations (the
sample remainder stays assumed-negligible). Because the calibration is
architecture-specific, mixed `.host` values are never pooled silently: the
caller picks a host or the function aborts listing them. Provenance records the
run-derived source and date.

### S3 objects mirror the cost-estimation pattern
`ssdsims_cost_analysis` and `ssdsims_cost_comparison` follow the existing
`ssdsims_cost_estimate` shape (difftime totals, `breakdown` tibble, provenance)
with `format`/`print` methods reusing `format_duration()`.

### Design-level rollup folds in here, gated on `scenario-combine`
The design-level forms of the three functions (accepting an `ssdsims_design` and
aggregating across its member scenarios) were drafted as a standalone
`cost-analysis-design` change and are **folded into this change** rather than
shipped separately: they belong to the same `cost-analysis` capability and reuse
this change's scenario-level functions, resolver, and objects. They are the last
tasks (group 9) and land after `scenario-combine`.

**Addressing — corrected to the landed `scenario-combine` (the drafted
`scenario=<name>` roots / `<name>_` prefix were never shipped).** As built,
`ssd_design_targets()` groups members by `seed` and writes one **shared**
`<root>/seed=<value>/layout=<hash>` tree per seed group (computed via the landed
`design_results_dir()`); a cell shared by several members is built **once**, and
the shard target names weave the `seed` in (`<step>_step_<seed>_<pathcell>`, from
`tar_map()` keyed on `c("seed", <path axes>)`) — there is no per-member root and
no `<name>_` shard prefix. Per-member observed cost is therefore recovered by
**filtering the shared shards to the member's `hc_id`/`fit_id`s** (exactly as
`ssd_summarise_member()` does), and the design store resolver regenerates the
seed-woven shard names **per seed group** (not per member). The combined
`<root>/summary.parquet` carries a `scenario` column and — because
`ssd_summarise_member()` projects out only `dists`/`samples` — retains the hc
timing columns, so it is a valid one-read fast path; the per-shard envelope is
computed once per shared shard (per seed group) since shards are shared. The
design total is **per-member accounting** (Σ members), so a shared cell counts
once per member — it answers "what each member costs", and can exceed the design's
once-built shared compute.

The aggregation is defined as a **collection-agnostic seam** (combine breakdowns,
reduce to design totals, pool measured frames host-aware) that the thin
`ssdsims_design` methods delegate to; a working, unit-tested prototype of that
seam is kept as proof of work under `exploration/design-rollup-seam/` (excluded
from the package build). Promote `combine_cost_breakdowns()`,
`design_cost_totals()`, `pool_calibration_from_frames()`, and
`format_design_breakdown()` into `R/` as-is; the prototype's
`design_member_addressing()` (which derived `scenario=<name>` roots and `<name>_`
prefixes) is **superseded** by the landed `design_results_dir()` + `hc_id`/`fit_id`
filtering and SHALL NOT be promoted. *Rejected alternative:* keep
`cost-analysis-design` standalone — it forked one capability across two changes
and shipped a seam ahead of its only consumer.

## Risks / Trade-offs

- [Forced recompute of a fit/hc shard with identical results now cascades:
  dependents re-run and `upload_<step>` re-ships, because the file hash includes
  volatile timing columns] → Scoped to fit/hc (sample unaffected); inner-axis
  growth changed bytes anyway; the §8.3 pin covers code-edit recomputes;
  documented in the factory's invalidation-model docs. Watch the re-upload arm
  at scale — egress on a forced refresh is the practical cost.
- [Schema change on fit/hc shards and summaries breaks existing readers/tests]
  → Pre-release package (0.0.0.9015, breaking allowed); oracle and
  atomic-rewrite tests move to result-column comparisons; snapshots update once.
- [Mixed-host results trees (cluster + local debugging shards) would corrupt a
  pooled calibration] → `.host` in-band; recalibration refuses silent pooling.
- [Regenerated target names may not match the store if the scenario or
  `partition_by` changed since the run] → Join-and-report: unmatched targets are
  surfaced with counts, never silently dropped, never fatal.
- [`tar_meta()` schema drift across targets versions] → Depend only on the
  long-stable `name`/`seconds` columns.
- [Clock skew across cluster nodes makes cross-node `.start` ordering
  approximate] → Durations are within-node differences (unaffected); document
  that cross-node Gantt reconstruction is approximate.

## Migration Plan

Additive API plus an output-schema change on the fit/hc layers. Order of
landing inside the change: (1) runner instrumentation + shard-runner spec delta
+ test migration to result-column identity; (2) analysis functions consuming the
columns; (3) docs/vignette. Pre-release, no compatibility shim: old results
trees simply lack the columns and route to the tar_meta fallback. Rollback is
reverting the runner edits and the new file; no on-disk migration exists either
way.

## Open Questions

- The hc longest-*task* is now measured, but the longest-*shard* (the actual
  dispatch unit under crew) comes from the envelope; the analysis print method
  should probably show both. Decide the exact print layout during implementation.
- Whether `ssd_compare_cost()` should also emit per-`ci_method` predicted/observed
  slope ratios (a finer diagnostic) — start with totals/longest and let the
  vignette's worked example decide.
