## Why

A declared scenario can fan out into a very expensive run with no warning. The
motivating scenario in `ssd_define_scenario()`'s docs — 10 sims × 4 `nrow` × 7
`ci_method` × 4 `nboot` (up to 50 000), `ci = TRUE` — was measured this session
at **~430 single-core hours** (~18 days), dominated by the hc bootstrap, with a
single `multi_fixed` + `nboot = 50 000` task taking ~44 minutes on its own. Users
need to know *before* launching: roughly how much total compute a scenario costs,
and how long the **single longest task** will take (the figure that sets cluster
wall-time and shard sizing).

That cost is predictable. Session benchmarking established a simple per-task
model: for `ci = TRUE` the hc bootstrap dominates, and per-hc-call time ≈
`base + slope(ci_method) × max(nboot, n0)`, with per-`ci_method` slopes spanning
~9× (`weighted_samples` ~5.7 ms/boot up to `multi_free` ~55), an `nboot` floor
`n0 ≈ 25–30`, a weak non-monotonic `nrow` factor, and `proportion`/`est_method`
effectively free (one bootstrap serves all). But those coefficients are
**architecture-specific** — they must be re-measured per machine to be
trustworthy.

So the package should ship a *method* (a calibration harness that re-fits the
coefficients on the target architecture) plus a *default estimator* (the
calibration measured in these sessions, with provenance), and an estimator
function that turns a scenario into a total-cost and longest-task prediction.

## What Changes

- **New `cost-estimation` capability.** Add a calibrate → estimate workflow:
  - **Calibration harness** `ssd_calibrate_cost()`: runs a small, fixed
    benchmark sweep on the *current* architecture — tiny `nboot` values, all
    `ci_method`s, a couple `nrow` — fits the per-task cost model
    (`time ~ base + slope(ci_method) × max(nboot, n0)`, plus the bounded `nrow`
    factor), and returns a versioned **calibration object** carrying the fitted
    coefficients and provenance (CPU, R/`ssdtools` versions, date, sweep grid).
  - **Estimator** `ssd_estimate_cost(scenario, calibration = ssd_cost_calibration())`:
    expands the scenario into its `hc` task table (read-only, via the existing
    task-list derivation), applies the fitted model per task, and returns the
    **ballpark total cost** and the **duration of the longest single task**,
    plus a per-axis breakdown (which `ci_method`/`nboot` cells dominate). It
    accounts for the free axes (one bootstrap per `nboot × ci_method ×
    parametric` cell; `proportion`/`est_method` do not multiply cost).
- **Ship a default calibration data object** (`ssd_cost_calibration()` accessor
  over a `data/`-shipped object) — the calibration fit in these sessions
  (Intel Xeon @ 2.10 GHz, R 4.5.3, `ssdtools` 2.6.0.9002) — used when the user
  has not run `ssd_calibrate_cost()` on their own machine. The estimate is
  labelled with the calibration's provenance and a clear "ballpark" caveat.
- **Reproducible analysis vignette** documenting the *method* (calibration sweep
  + linear model, the `max(nboot, n0)` form, why `proportion`/`est_method` are
  free, the non-monotonic `nrow` factor) and the *outcome* (the estimator and a
  worked example on the motivating scenario), rerunnable end-to-end to produce a
  custom estimator for any architecture.

No change to scenario results, RNG, task graph, or shard layout: the estimator
only *reads* the scenario's task expansion.

## Capabilities

### New Capabilities
- `cost-estimation`: calibrate a per-task cost model on the target architecture
  and estimate a scenario's total computation cost and longest-task duration,
  with a shipped default calibration and a rerunnable calibration workflow.

### Modified Capabilities
<!-- None: read-only use of the existing run-scenario / task-lists expansion. -->

## Impact

- **New code**: `R/cost-estimate.R` (`ssd_calibrate_cost()`,
  `ssd_estimate_cost()`, `ssd_cost_calibration()`, an `ssdsims_cost_calibration`
  S3 object + `print`/`format`), reusing `ssd_scenario_hc_tasks()` /
  `task_axes("hc")` read-only.
- **New data**: `data/ssd_cost_calibration_default.rda` (or `inst/extdata/`) with
  the session calibration + provenance; documented and added to `_pkgdown.yml`.
- **New vignette**: `vignettes/cost-estimation.qmd` (the reproducible analysis).
- **Docs**: `man/` for the new functions/data; reference index entry.
- **Dependencies**: none new (uses existing `ssdtools`, `dplyr`, `chk`,
  `tibble`); benchmarking uses base `system.time`.
- **Risk**: estimates are ballpark and architecture-dependent — mitigated by the
  shipped provenance, the explicit caveat, and the rerunnable calibration; the
  model intentionally targets order-of-magnitude/wall-time sizing, not exact
  timing.
