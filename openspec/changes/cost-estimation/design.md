## Context

Scenarios are declarative and can fan out into multi-day runs invisibly. The hc
bootstrap dominates `ci = TRUE`; the fit step and data sampling are comparatively
cheap. Session benchmarking (Intel Xeon @ 2.10 GHz, R 4.5.3, `ssdtools`
2.6.0.9002) produced the per-task model and these coefficients (one bootstrap per
`nboot × ci_method × parametric` cell; `proportion`/`est_method` free):

| ci_method | n0 (boot floor) | base (s) | ms/boot |
| --- | --- | --- | --- |
| weighted_samples | 0 | 0.62 | 5.68 |
| GMACL | 25 | 1.02 | 18.90 |
| MACL | 25 | 0.99 | 19.64 |
| arithmetic_samples | 30 | 1.13 | 21.24 |
| geometric_samples | 25 | 1.04 | 21.60 |
| multi_fixed | 0 | 0.07 | 52.63 |
| multi_free | 25 | 1.16 | 55.00 |

The `nrow` effect is weak and non-monotonic (cheap at 5 where most bcanz dists
fail to fit, peaking ~10–20, easing at 50) — not a clean linear axis. The
existing read-only task expansion (`ssd_scenario_hc_tasks()`,
`R/task-lists.R:106`) already yields one row per hc task with `nboot`,
`ci_method`, `parametric`, `nrow`, etc., which is exactly the grain the estimator
needs.

These coefficients are architecture-specific, so the package must ship both a
*method* to re-measure them and a *default* fitted here.

## Goals / Non-Goals

**Goals:**
- A calibration object (`ssdsims_cost_calibration`) carrying per-`ci_method`
  coefficients + provenance, produced by `ssd_calibrate_cost()` on any machine.
- `ssd_estimate_cost(scenario)` returning ballpark **total** cost and **longest
  single task** duration + a per-axis breakdown, purely by reading the task
  expansion (no execution).
- A shipped default calibration (this session) and a rerunnable analysis vignette
  documenting method and outcome.

**Non-Goals:**
- Exact timing or a performance model of the fit/sample steps (bootstrap
  dominates `ci = TRUE`; treat the rest as a small fixed per-task addend).
- Parallel/cluster scheduling simulation — the estimator reports *serial* total
  and the *single* longest task; turning those into wall-time under N workers is
  left to the caller (longest task is the irreducible floor).
- Modelling `ci = FALSE` runs in detail (no bootstrap → cheap; reported as the
  fixed analytical addend).

## Decisions

### Decision 1: Estimate from the read-only hc task table, not by running
`ssd_estimate_cost()` expands the scenario via `ssd_scenario_hc_tasks()` and maps
the calibrated model over rows. The task table already encodes the corrected
free-axis semantics (after `est-method-setting`, `est_method` is not a fan-out
axis), so the estimator inherits "one bootstrap per `nboot × ci_method ×
parametric` cell" for free and stays correct as the task model evolves.

*Alternative:* re-derive the cross-join inside the estimator — rejected
(duplicates `task_axes`/expansion logic, drifts from the real task graph).

### Decision 2: Model `time = (base + slope · max(nboot, n0)) · nrow_factor`
Per `ci_method`, fit `lm(time ~ pmax(nboot, n0))` with `n0` chosen by minimising
residual SD over a small grid (the method used this session). The `nrow` factor
is a bounded lookup/interpolation (≈0.2 at 5, ≈1 at 10–20, ≈0.7 at 50) captured
during calibration, **not** a linear term — its non-monotonicity is real and
data-dependent. `parametric` enters as a small multiplier if calibration finds
one; otherwise 1.

*Alternative:* a single global regression with `ci_method` dummies — rejected;
the 9× slope spread and per-method `n0` are better captured per `ci_method`, and
keep the shipped coefficients human-auditable.

### Decision 3: `ssdsims_cost_calibration` is a small, serialisable S3 object
Fields: a per-`ci_method` coefficient table (`base`, `slope`, `n0`), the
`nrow_factor` table, a `fixed_addend` (sample+fit per-task overhead), and
`provenance` (cpu, `R.version.string`, `ssdtools` version, date, sweep grid).
`print`/`format` methods render coefficients + provenance + a ballpark caveat.
Shipped as `data/` (lazy-loaded) with `ssd_cost_calibration()` as the accessor so
the default can be swapped without touching call sites.

*Alternative:* store as `inst/extdata` CSV — rejected; an S3 object with methods
documents and validates itself and round-trips coefficients without parsing.

### Decision 4: Calibration sweep is small, fixed, and self-contained
`ssd_calibrate_cost()` fits one bcanz model per `nrow ∈ {default small set}` and
times `ssdtools::ssd_hc()` over tiny `nboot` (e.g. {20, 50, 100, 200}) × all
`ci_method`s — the exact protocol used this session (≈minutes, not the hours a
real scenario costs). It draws data by resampling a reference dataset
(`ssddata::ccme_boron`) so it is dependency-light and reproducible.

### Decision 5: Report serial total and longest task; expose the breakdown
Total = Σ per-task time; longest = max per-task time. The breakdown groups by
`ci_method` × `nboot` so users see which cells dominate (e.g. `multi_free` ×
`nboot = 50 000`). Wall-time under parallelism is `max(longest_task,
total / n_workers)` — documented, computed by the caller.

## Risks / Trade-offs

- **Estimates are ballpark and machine-specific** → ship provenance, print a
  caveat, and make recalibration a one-call, documented step. The model targets
  order-of-magnitude/wall-time sizing, not stopwatch accuracy.
- **`nrow` non-monotonicity / data dependence** → captured as a bounded factor
  from calibration, not extrapolated linearly; flagged as the least precise
  part of the model.
- **`ssdtools` performance changes across versions** → provenance records the
  `ssdtools` version; a stale default is visible in the printed estimate, and
  recalibration corrects it.
- **Convergence variance inflates per-call times** → calibration uses small
  `nboot` with the same protocol that gave r² ≈ 0.5–0.6 on the slope; the
  estimate is explicitly ballpark, and total cost averages out per-task noise.

## Migration Plan

1. Land the `cost-estimation` spec.
2. Implement the S3 object + `ssd_calibrate_cost()` / `ssd_estimate_cost()` /
   `ssd_cost_calibration()` in `R/cost-estimate.R`.
3. Generate and check in the default calibration `data/` object (this session's
   coefficients + provenance) via a `data-raw/` script.
4. Write the reproducible vignette; add docs, `_pkgdown.yml`, and tests
   (estimation runs no RNG/fit; free axes don't multiply; longest task tracks the
   costliest cell).

No rollback concerns — additive, read-only feature.

## Open Questions

- Should the default sweep grid live in the shipped calibration (so `print`
  shows exactly how it was measured) or in package constants? (Leaning: in the
  object, for provenance.)
- Should `ssd_estimate_cost()` optionally accept `n_workers` and report wall-time
  directly, or leave that arithmetic to the caller? (Leaning: return the serial
  total + longest task; document the wall-time formula.)
- Is a tiny fit/sample-step addend worth calibrating, or is the bootstrap-only
  model sufficient for `ci = TRUE`? (Leaning: a single measured fixed addend.)
