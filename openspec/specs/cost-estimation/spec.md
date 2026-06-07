# cost-estimation Specification

## Purpose

Predict, before a scenario is launched, roughly how much total compute it costs
and how long its single longest task will take — the figures that set cluster
wall-time and shard sizing. The capability ships a *method* (a calibration
harness that re-fits an architecture-specific per-task cost model) and a
*default estimator* (the calibration measured during development, with
provenance), plus a read-only estimator that turns an `ssdsims_scenario` into a
total-cost and longest-task prediction without running any fit or bootstrap.

## Requirements

### Requirement: Per-task cost model
The package SHALL define a per-task cost model for `ci = TRUE` hc tasks of the
form `time ≈ base(ci_method) + slope(ci_method) × max(nboot, n0(ci_method))`,
multiplied by a bounded `nrow` factor, where the bootstrap dominates and
`proportion` and `est_method` contribute no per-task multiplier (one bootstrap
per `nboot × ci_method × parametric` cell serves every `proportion` and
`est_method`). The model's coefficients SHALL be carried by a calibration object
and SHALL be architecture-specific.

#### Scenario: Free axes do not multiply the estimate
- **WHEN** a scenario differs only in the number of `proportion` or `est_method` values
- **THEN** the estimated total cost SHALL be unchanged (those axes do not multiply bootstrap work)

#### Scenario: Cost scales with max(nboot, n0)
- **WHEN** two tasks share a `ci_method` but have `nboot` values both at or below `n0`
- **THEN** their estimated per-task durations SHALL be equal (the floor applies), and above `n0` the duration SHALL increase linearly in `nboot`

### Requirement: Calibration harness re-fits the model on the target architecture
The package SHALL expose `ssd_calibrate_cost()` that runs a small, fixed
benchmark sweep on the current machine — over tiny `nboot` values, all
`ssdtools::ssd_ci_methods()`, and at least two `nrow` values — times each hc
call, fits the per-task cost model, and returns a versioned
`ssdsims_cost_calibration` object. The object SHALL carry the fitted
per-`ci_method` coefficients and provenance: CPU description, `R` version,
`ssdtools` version, date, and the sweep grid used.

#### Scenario: Calibration returns a provenance-carrying object
- **WHEN** `ssd_calibrate_cost()` completes on a machine
- **THEN** it SHALL return an `ssdsims_cost_calibration` object whose fields include per-`ci_method` `base`/`slope`/`n0` coefficients and provenance (CPU, R version, ssdtools version, date, sweep grid)

#### Scenario: Calibration is rerunnable to yield a custom estimator
- **WHEN** `ssd_calibrate_cost()` is run on a different architecture and passed to `ssd_estimate_cost()`
- **THEN** the estimates SHALL use that architecture's coefficients rather than the shipped default

### Requirement: Shipped default calibration
The package SHALL ship a default `ssdsims_cost_calibration` object, accessible via
`ssd_cost_calibration()`, fitted during package development, with its provenance
recorded. `ssd_estimate_cost()` SHALL use this default when no calibration is
supplied, and SHALL surface the calibration's provenance and a ballpark caveat
with the estimate.

#### Scenario: Default used when none supplied
- **WHEN** `ssd_estimate_cost(scenario)` is called without a `calibration` argument
- **THEN** it SHALL use `ssd_cost_calibration()` (the shipped default) and the returned estimate SHALL be annotated with that calibration's provenance

### Requirement: Scenario cost and longest-task estimator
The package SHALL expose `ssd_estimate_cost(scenario, calibration =
ssd_cost_calibration())` that accepts an `ssdsims_scenario`, expands it into its
hc task table read-only (via the existing task-list derivation, without running
any fit or bootstrap), applies the calibrated per-task cost model, and returns
both the **ballpark total computation cost** (summed serial task time) and the
**duration of the single longest task**, together with a per-axis breakdown
identifying the dominant `ci_method`/`nboot` cells. The estimator SHALL NOT
execute the scenario, draw random numbers, or alter any result.

#### Scenario: Returns total cost and longest-task duration
- **WHEN** `ssd_estimate_cost()` is called on a scenario with `ci = TRUE`
- **THEN** the result SHALL report a total estimated compute duration and the estimated duration of the longest single task, both as time quantities

#### Scenario: Estimation does not run the scenario
- **WHEN** `ssd_estimate_cost()` is called
- **THEN** no distributions SHALL be fitted, no bootstrap SHALL run, `.Random.seed` SHALL be unchanged, and no files SHALL be written

#### Scenario: Longest task reflects the costliest cell
- **WHEN** a scenario includes a `multi_fixed`/`multi_free` `ci_method` at the largest `nboot`
- **THEN** the reported longest-task duration SHALL correspond to that cell (the costliest per the calibrated slopes), not merely the most numerous cell

### Requirement: Recalibration is a single function call
Producing an architecture-specific estimator SHALL require nothing more than
calling `ssd_calibrate_cost()` and passing its result to `ssd_estimate_cost()`;
the calibration logic SHALL live in that function (not in a vignette code-block
or a standalone script the user must run). The package MAY document the
discovery of the model's *form* (the session sweeps) under the change's
`exploration/` directory, but that material is illustrative only and SHALL NOT
be required to recalibrate.

#### Scenario: A user recalibrates with one call
- **WHEN** a user wants a custom estimator for their machine
- **THEN** calling `ssd_calibrate_cost()` SHALL return a usable `ssdsims_cost_calibration` without running any separate analysis script

### Requirement: Cost-estimation documentation vignette
The package SHALL include a vignette that documents the method (the
`max(nboot, n0)` per-`ci_method` model, why `proportion`/`est_method` are free,
the bounded non-monotonic `nrow` factor) and demonstrates `ssd_calibrate_cost()`
and `ssd_estimate_cost()` on a worked scenario. The vignette is documentation; it
SHALL NOT be the mechanism by which calibration is reproduced.

#### Scenario: Vignette demonstrates the workflow
- **WHEN** the cost-estimation vignette is rendered
- **THEN** it SHALL call `ssd_calibrate_cost()` and `ssd_estimate_cost()` and display a total-cost and longest-task estimate for a worked scenario
