## Context

The `cost-estimation` capability (`R/cost-estimate.R`) predicts a scenario's
compute cost from a per-`ci_method` model `time = (base + slope × max(nboot, n0))
× nrow_factor(nrow) + fixed_addend`, calibrated by a synthetic micro-benchmark
(`ssd_calibrate_cost()`) that never touches the real pipeline. A real run is the
best calibration data available, but the runners discard the per-task durations
that would make it usable, and the `targets` meta store only records wall seconds
per *target* (shard) — too coarse to separate tasks with different
`nboot`/`ci_method` inside one `hc` shard.

This revision supersedes the full-fat proposal (#152), which added two bespoke
S3 objects (`ssdsims_cost_analysis`, `ssdsims_cost_comparison`) and three
functions (`ssd_analyse_cost()`, `ssd_compare_cost()`,
`ssd_calibrate_cost_from_run()`) plus a `tar_meta` envelope resolver. The
descoping rationale is below.

Constraints that shaped the design: deliverable-scale scenarios run to ~450k
shard files, so the shard-file count is the object budget; the shard-runner spec
pins per-task results byte-identical to the in-memory baseline oracle; the
invalidation/upload models content-hash the `format = "file"` Parquets; the
per-scenario manifest is parked (no sidecar provenance mechanism is revived).

## Goals / Non-Goals

**Goals:**
- Capture per-task `hc`/`fit` durations in-band (`.start`/`.end`/`.host`) so a
  run summary is sufficient calibration input.
- Add exactly one function — calibrate the existing model from a run summary —
  closing *result summary → estimation* through existing types.
- Keep observed-cost totalling/ranking and predicted-vs-observed comparison as
  documentation (`dplyr` recipes), not code.

**Non-Goals:**
- New S3 objects, an `analyse` function, or a `compare` function.
- A `tar_meta`-store reader / shard-envelope resolver (a documented join recipe
  if anyone needs per-shard overhead; not a shipped function).
- Sidecar files or any per-shard provenance artifact (no manifest revival).
- Timing the `sample` step (cheap; stays in `fixed_addend`; keeps the sample
  layer file-level deterministic).
- Instrumenting the legacy `ssd_*_sims` family (waits for `migrate-public-api`).
- Changing the cost model's *form* or `ssd_estimate_cost()`.

## Decisions

### Descope to one function; comparison and observed-aggregation are documentation
The observed-cost "analysis object" was essentially *the summary minus samples* —
a tibble the run already produces (`ssd_summarise()` output with timing columns).
Wrapping it in `ssdsims_cost_analysis` added a type that holds nothing the
summary tibble doesn't, and `ssd_compare_cost()` was a join with a ratio column.
Both are data-frame analysis, served by a vignette. The one piece that earns code
is fitting: the `n0` grid-search + per-`ci_method` `lm()` + bounded `nrow_factor`
protocol already exists internally for the synthetic path
(`calibrate_coefficients()`, `calibrate_nrow_factor()`); exposing it for observed
data (returning the existing `ssdsims_cost_calibration`) is the high-value,
low-surface addition and the only thing users would otherwise reimplement.
*Rejected:* keeping the analyse/compare objects (API surface for one-liners);
folding everything to docs including the fit (forces users to reimplement a
non-trivial protocol).

### Timing columns in-band, not sidecars
A per-shard `timings.parquet` sidecar would keep `part.parquet` byte-stable but
**doubles the object count** (~450k → ~900k files): inode pressure, blob-store
object counts, and every glob/listing pay for a few bytes per task. In-band
columns add zero files, survive the future `shard-failure-survival` shorter-shard
semantics for free (survivors' timings are surviving rows), and travel with
uploads. *Rejected:* sidecars (file count); the `targets` store alone (shard
granularity only, prunable, doesn't travel, can't split a multi-task shard).

### Byte-identity narrows to result columns — fit/hc only
The shard-runner contract is restated over **result columns** joined on
`<step>_id`, with `.start`/`.end`/`.host` enumerated out (a `shard-runner` delta).
`sample` is untimed, so sample shards keep file-level determinism. The cost is
that a `fit`/`hc` shard's **file hash is no longer deterministic across
recomputes**: a forced recompute with identical results re-runs dependents and
re-uploads (Risks). Traced against §8, this loss is narrow — inner-axis growth
changes result bytes anyway, and the code-edit-without-result-change case is what
the §8.3 pin (`tar_cue(depend = FALSE)`) exists for.

### `.start`/`.end` as UTC timestamps, `.host` as the CPU description
Start+end (not duration-only) is the same storage and strictly richer (a
per-worker Gantt, straggler detection, whether the longest task gated wall time).
Stored as Parquet TIMESTAMP (UTC); duration derived. `.host` is the existing
`cost_cpu_info()` description — the grain the architecture-specific calibration
pools on (identical cluster nodes pool together) and what lets
`ssd_calibrate_cost_from_run()` record provenance from the run itself. On `hc`
the values repeat across `proportion` rows and RLE-compress to ~nothing.

### Capture in the runners, not the task primitives
The `*_data_task_primer()` primitives return domain objects; bracketing each task
in the step-runner loops (and the baseline's fit/hc loops) keeps the primitives
pure and the capture in one place per runner. `Sys.time()` is RNG-neutral and its
resolution is ample against `fixed_addend`-scale (~0.05 s) tasks.

### Summary is the calibration input; the function takes the summary, not a store
`ssd_summarise()` retains the timing columns, so `ssd_calibrate_cost_from_run()`
consumes the summary tibble (or Parquet path) the user already has — no `targets`
dependency, works after upload when shards are remote. The fit/estimate split is
kept (vs a bundled `ssd_estimate_cost_from_run()`): it matches the existing
`ssd_calibrate_cost()`/`ssd_estimate_cost()` shape and lets one run calibrate many
scenario estimates.

### Host-aware, never silent pooling
Because the calibration is architecture-specific, a summary spanning multiple
`.host` values (cluster + local debugging shards) is refused: explicit host
choice or abort listing them, rather than averaging incomparable machines.

## Risks / Trade-offs

- [Forced recompute of a fit/hc shard with identical results cascades: dependents
  re-run and `upload_<step>` re-ships, because the file hash includes volatile
  timing columns] → Scoped to fit/hc (sample unaffected); inner-axis growth
  changed bytes anyway; the §8.3 pin covers code-edit recomputes; documented in
  the factory's invalidation docs. Watch the re-upload egress on a forced refresh.
- [Schema change on fit/hc shards and summaries breaks readers/tests] →
  Pre-release (0.0.0.9015); oracle and atomic-rewrite tests move to result-column
  comparisons; snapshots update once.
- [Mixed-host results trees would corrupt a pooled calibration] → `.host` in-band;
  the function refuses silent pooling.
- [Clock skew across cluster nodes makes cross-node `.start` ordering approximate]
  → Durations are within-node differences (unaffected); only cross-node Gantt
  reconstruction (a documentation nicety) is approximate.

## Migration Plan

Additive API (one function) plus an output-schema change on the fit/hc layers.
Landing order inside the change: (1) runner instrumentation + `shard-runner`
delta + test migration to result-column identity; (2)
`ssd_calibrate_cost_from_run()`; (3) docs/vignette. Pre-release, no compatibility
shim: old results trees simply lack the columns and cannot be calibrated from
(the synthetic `ssd_calibrate_cost()` remains). Rollback is reverting the runner
edits and the function; no on-disk migration either way.

## Open Questions

- Should `ssd_calibrate_cost_from_run()` accept a data frame, a Parquet path, or
  both? Leaning both (a thin `is.character` read), decided at implementation.
- Is the measured `fit`-duration `fixed_addend` worth deriving, or is the
  synthetic-default addend close enough? The fit timing is the same bracket cost
  either way; the worked vignette can show whether the measured addend moves the
  estimate enough to matter.
