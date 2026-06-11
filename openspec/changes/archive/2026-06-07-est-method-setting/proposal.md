## Why

`est_method` is currently an **hc cross-join axis**: `task_axes("hc")`
(`R/task-lists.R:379`) lists it, and `ssd_hc_sims()` (`R/hc-sims.R:75`) crosses
the input with `expand_grid(nboot, est_method, ci_method, parametric)`, running a
**separate** `ssdtools::ssd_hc()` bootstrap per combination (seeded only by
`(sim, stream, seed)`, `R/internal.R:143`).

Benchmarking in this session shows that is wasted work. `arithmetic`,
`geometric`, and `multi` are all **post-hoc aggregations of the same bootstrap
sample set**: with a fixed seed, `ssd_hc(..., est_method = "arithmetic")` and
`est_method = "multi"` return **byte-identical** `samples` and differ only in the
aggregated `est`/CI columns (verified). Their per-call cost is identical
(arithmetic 6.04 s / geometric 5.87 s / multi 5.77 s at `nboot = 1000`). So a
3-value `est_method` axis pays the bootstrap **3×** to recompute the same draws
and re-summarise them — a pure ~3× overhead on the most expensive step. On the
motivating scenario (10 sims × 4 `nrow` × 7 `ci_method` × 4 `nboot`,
`ci = TRUE`) collapsing `est_method` cuts the single-core estimate from ~430 h to
~143 h. Point estimates (`est`) are unchanged (analytical); CIs are re-seeded by
the axis change (see Impact) and change numerically while staying statistically
equivalent.

This is the same re-classification `dists-scenario-setting` applied to `dists`,
but unlike that one (a label-only fix — `dists` was never an axis in code) this
change **removes a real fan-out**: `est_method` is computed-together, like
`proportion` already is (one `ssd_hc()` call serves a whole `proportion` vector,
and `proportion` is absent from `task_axes("hc")`).

## What Changes

- **Re-classify `est_method` as an hc-level scenario setting**, not a
  cross-join axis. Like `proportion`, it becomes a within-result dimension: one
  bootstrap per `(dataset, sim, replace, nrow, fit-grid, nboot, ci_method,
  parametric)` cell yields **all** requested `est_method` summaries.
- **Drop `est_method` from `task_axes("hc")`** (`R/task-lists.R`). The hc
  fan-out becomes `nboot × ci_method × parametric` when `ci = TRUE`, and a single
  hc row per fit task when `ci = FALSE` (today `est_method` is "the only fan-out
  axis" in that case — afterwards there is none).
- **Compute the est_method summaries from one retained sample set** in
  `ssd_hc_sims()`/`hc_state()` (`R/hc-sims.R`, `R/internal.R`): run the bootstrap
  once with `samples = TRUE`, then derive each `est_method`'s `est`/`se`/`lcl`/`ucl`
  from those samples (the per-distribution draws), rather than calling
  `ssd_hc()` once per method. Result rows remain one-per-`est_method` (it stays a
  column on each `hc` tibble).
- **Move `est_method` in the `ssd_define_scenario()` signature** out of the hc
  grid block into the contiguous scenario-settings block, beside
  `proportion`/`ci`/`samples`; store it under `scenario$hc` as a setting, not an
  axis. `print.ssdsims_scenario()` renders it among the hc settings.
- **Update `partition_by`/`bundle` vocabulary** so `est_method` is no longer an
  accepted `hc` path/inner axis (it is not in `task_axes("hc")`).
- **Sweep call sites and docs** (examples, tests, snapshots, scripts,
  vignettes, `inst/targets-templates/`, `man/`, `GLOSSARY.md`/`TARGETS-DESIGN.md`).

Point estimates (`est`) are **unchanged** (analytical, seed-independent). The
bootstrap CIs are **re-seeded** by the axis change — the hc primer hashes the
hc-grid row, which today includes `est_method` (`R/task-primer.R`), so dropping
it changes every hc task's primer. The new CIs are statistically equivalent and,
unlike today, consistent across est_methods within a task. The byte-identity of
est_method as a pure post-hoc aggregation holds **at a fixed seed** and is
demonstrated by `exploration/est-method-invariance.R`; it is not asserted
post-hoc against the differently-seeded old pipeline.

## Capabilities

### New Capabilities
<!-- None: this corrects/extends existing capabilities. -->

### Modified Capabilities
- `hazard-concentrations`: `est_method` leaves the factorial-expansion
  requirement; a new requirement states that all requested `est_method`
  summaries are derived from a **single** bootstrap sample set (no per-method
  re-bootstrap), with byte-identical estimates.
- `task-lists`: the hc task table no longer fans out over `est_method`
  (`task_axes("hc")` drops it); the `ci = TRUE` fan-out is `nboot × ci_method ×
  parametric`, and `ci = FALSE` yields one hc row per fit task.
- `scenario-definition`: `est_method` is an hc-level **scenario setting** in
  the role-grouping requirement and the signature's settings block, stored under
  `scenario$hc`.

## Impact

- **Specs**: `hazard-concentrations`, `task-lists`, `scenario-definition` deltas.
- **Code**: `R/hc-sims.R` (drop `est_method` from `expand_grid`; bootstrap once,
  summarise per method), `R/internal.R` (`hc_state()`/`hc_seed()` produce
  multi-method output from retained samples), `R/task-lists.R` (`task_axes("hc")`),
  `R/scenario.R` (signature/storage/print), `partition_by`/`bundle` validation.
- **RNG re-seeding**: the hc primer hashes the hc-grid row including
  `est_method` (`R/task-primer.R:91-94`), so dropping the axis re-seeds every hc
  task. Point estimates (`est`) are unchanged; bootstrap CIs change numerically
  (statistically equivalent). No byte-identity is preserved across the axis
  change, so no stored-CI migration is implied.
- **Correctness gate**: a same-seed invariant unit test (one bootstrap →
  per-method analytical `est` + shared CI equals per-method `ssd_hc()` calls
  seeded with the *same* primer), backed by `exploration/est-method-invariance.R`
  — **not** an old-vs-new pipeline equality test (the seeds differ).
- **Call sites/docs**: name-only signature move plus snapshot re-recording for
  printed scenarios, `hc`-row-count assertions that assumed an `est_method`
  fan-out, and any CI snapshots affected by re-seeding.
- **Cost**: ~3× reduction on the `est_method` axis; point estimates unchanged.
