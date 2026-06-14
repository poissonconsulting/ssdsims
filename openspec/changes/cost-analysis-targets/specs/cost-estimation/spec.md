## ADDED Requirements

### Requirement: Calibrate the cost model from a run's result summary
The package SHALL expose `ssd_calibrate_cost_from_run(summary, ...)` that fits
the **existing** per-task cost model from a completed run's result summary — the
tibble (or Parquet path) `ssd_summarise()` produces, carrying per-`hc`-row axis
columns (`nrow`, `ci_method`, `nboot`) and the `.start`/`.end`/`.host` timing
columns — and returns an `ssdsims_cost_calibration` of the same shape and fitting
protocol `ssd_calibrate_cost()` produces (per-`ci_method` `base`/`slope`/`n0`, a
bounded `nrow_factor`, a `fixed_addend`). It SHALL derive per-task `hc` durations
from `.end − .start`, reuse the model's existing fitting logic (the `n0`
selection and per-`ci_method` regression, and the bounded `nrow_factor`
derivation), and derive `fixed_addend` from the run's measured `fit` durations
where available. The provenance SHALL be read from the run (`.host` as the CPU,
the observed-run date) and SHALL mark the calibration run-derived, distinguishing
it from a synthetic-sweep result. The returned object SHALL be usable directly by
the unchanged `ssd_estimate_cost(scenario, calibration)`. The function SHALL be
read-only: it SHALL NOT run any pipeline, fit any distribution, draw random
numbers, or write any file, and SHALL NOT require the `targets` package.

#### Scenario: Run-derived calibration feeds the estimator
- **WHEN** `ssd_calibrate_cost_from_run()` is called on a run summary and its result is passed to `ssd_estimate_cost()`
- **THEN** the estimate SHALL use the run's measured coefficients rather than the shipped default, with no other change to `ssd_estimate_cost()`

#### Scenario: Calibration is read-only and targets-free
- **WHEN** `ssd_calibrate_cost_from_run()` is called
- **THEN** no pipeline SHALL run, `.Random.seed` SHALL be unchanged, no file SHALL be written, and the call SHALL NOT require `targets`

#### Scenario: Mixed hosts are not silently pooled
- **WHEN** the run summary carries more than one distinct `.host`
- **THEN** the function SHALL NOT pool them silently; it SHALL require an explicit host choice or abort naming the hosts found

#### Scenario: Provenance marks the calibration as run-derived
- **WHEN** `ssd_calibrate_cost_from_run()` completes
- **THEN** the returned `ssdsims_cost_calibration`'s provenance SHALL identify it as derived from a run (the `.host` CPU and the observed-run date), distinguishing it from a synthetic-sweep result

### Requirement: Documentation of the result-summary calibration loop
The cost-estimation documentation SHALL demonstrate, as documentation (not as
package functions), the loop from a completed run to a refined estimate: reading
the run summary, totalling and ranking observed per-task durations with `dplyr`
(`.end − .start`), calibrating from it via `ssd_calibrate_cost_from_run()`,
re-estimating with `ssd_estimate_cost()`, and placing predicted beside observed
as a `dplyr` join. The package SHALL NOT add bespoke `analyse`/`compare`
functions or objects for this; the comparison is data-frame analysis the user
performs against the summary the run already produces.

#### Scenario: Documentation shows summary → calibrate → estimate → compare
- **WHEN** the cost-estimation documentation is rendered
- **THEN** it SHALL show reading a run summary, calling `ssd_calibrate_cost_from_run()` and `ssd_estimate_cost()`, and a `dplyr` comparison of observed durations against the estimate — without invoking any `ssd_analyse_cost()`/`ssd_compare_cost()` function (no such functions exist)
