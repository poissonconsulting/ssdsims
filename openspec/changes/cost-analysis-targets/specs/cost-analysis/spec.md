## ADDED Requirements

### Requirement: Observed cost analysis from a targets store
The package SHALL expose `ssd_analyse_cost(scenario, store)` that reads a
completed `targets` run's meta store (via `targets::tar_meta()`), extracts the
per-target `seconds`, attributes each shard target's duration to the
`ssdsims_scenario` shard that produced it, and returns an `ssdsims_cost_analysis`
object. The object SHALL carry the observed total compute (summed serial target
seconds), the observed longest single shard duration, and a per-axis breakdown
keyed the same way `ssd_estimate_cost()`'s breakdown is (`ci_method` × `nboot`).
The function SHALL be read-only: it SHALL NOT run the pipeline, fit any
distribution, draw random numbers, or write any file. It SHALL default `store`
to `targets::tar_config_get("store")`.

#### Scenario: Returns observed total and longest from the store
- **WHEN** `ssd_analyse_cost()` is called on a scenario whose run was recorded in `store`
- **THEN** the result SHALL report the observed total compute duration and the observed longest single shard duration, both as time quantities, computed from the store's `seconds` field

#### Scenario: Analysis does not execute the scenario
- **WHEN** `ssd_analyse_cost()` is called
- **THEN** no distributions SHALL be fitted, no bootstrap SHALL run, `.Random.seed` SHALL be unchanged, and no files SHALL be written

#### Scenario: Errored or skipped targets do not break the analysis
- **WHEN** the store contains a target with a missing or `NA` `seconds` value (an errored or not-yet-built shard)
- **THEN** `ssd_analyse_cost()` SHALL exclude that target from the totals rather than abort, and SHALL report how many shard targets contributed observed durations

### Requirement: Shard target names resolve back to scenario shards
The package SHALL map a `tar_map`-minted shard target name of the form
`<step>_step_<pathcell>` back to its step and its `partition_by` path-cell axis
values, using the scenario's shard layout as the source of truth, so observed
durations attribute to the correct step (`sample`/`fit`/`hc`) and partition cell
without re-running anything. The resolver SHALL match the target names the
pipeline actually mints (the same names `ssd_scenario_targets()` produces), and
SHALL ignore non-shard targets (e.g. `summary`, `upload_<step>`) for the per-task
cost attribution.

#### Scenario: A shard target attributes to its step and path cell
- **WHEN** the store contains a shard target named for a known `partition_by` path cell of a step
- **THEN** the resolver SHALL identify that target's step and the axis values of its shard, matching the shard `ssd_scenario_<step>_shards()` produced for that scenario

#### Scenario: Non-shard targets are excluded from per-task attribution
- **WHEN** the store contains `summary` and `upload_<step>` targets alongside the shard targets
- **THEN** those targets SHALL NOT be attributed to scenario shards in the per-axis cost breakdown

### Requirement: Recalibrate the cost model from an observed run
The package SHALL expose `ssd_calibrate_cost_from_run(scenario, store)` that
re-fits the per-task cost model from an observed run's `hc`-shard durations and
returns an `ssdsims_cost_calibration` object of the same shape
`ssd_calibrate_cost()` returns (per-`ci_method` `base`/`slope`/`n0`, a bounded
`nrow_factor`, a `fixed_addend`, and provenance). The provenance SHALL record
that the calibration was derived from a run (including the store path and the
observed-run date) rather than from the synthetic `ssd_calibrate_cost()` sweep,
so a consumer can tell a run-derived calibration from a sweep-derived one. The
result SHALL be usable directly by `ssd_estimate_cost()`.

#### Scenario: Run-derived calibration is usable by the estimator
- **WHEN** `ssd_calibrate_cost_from_run()` returns a calibration and it is passed to `ssd_estimate_cost()`
- **THEN** the estimate SHALL use that run's fitted coefficients rather than the shipped default

#### Scenario: Provenance marks the calibration as run-derived
- **WHEN** `ssd_calibrate_cost_from_run()` completes
- **THEN** the returned object's provenance SHALL identify it as derived from a targets run (carrying the store path), distinguishing it from a `ssd_calibrate_cost()` sweep result

### Requirement: Compare predicted against observed cost
The package SHALL expose `ssd_compare_cost(scenario, store, calibration)` that
places the predicted estimate (`ssd_estimate_cost(scenario, calibration)`) beside
the observed analysis (`ssd_analyse_cost(scenario, store)`) and returns an
`ssdsims_cost_comparison` object reporting, at minimum, the predicted and observed
total compute, the predicted and observed longest task/shard, and the ratio of
predicted to observed for each. The comparison SHALL be read-only.

#### Scenario: Comparison reports predicted, observed, and their ratio
- **WHEN** `ssd_compare_cost()` is called on a scenario with a recorded run
- **THEN** the result SHALL report the predicted total, the observed total, and their ratio, and likewise for the longest task/shard

#### Scenario: Comparison runs no pipeline
- **WHEN** `ssd_compare_cost()` is called
- **THEN** no pipeline SHALL run, no random numbers SHALL be drawn, and no file SHALL be written

### Requirement: Cost-analysis documentation vignette
The package SHALL include a vignette that demonstrates the analyse → compare →
recalibrate loop: running (or loading) a small scenario through the `targets`
pipeline, calling `ssd_analyse_cost()` to read observed durations, `ssd_compare_cost()`
to place them beside the prediction, and `ssd_calibrate_cost_from_run()` to derive
a run-based calibration. The vignette is documentation; it SHALL NOT be the
mechanism by which analysis or recalibration is performed.

#### Scenario: Vignette demonstrates the loop
- **WHEN** the cost-analysis vignette is rendered
- **THEN** it SHALL call `ssd_analyse_cost()` and `ssd_compare_cost()` and display an observed total, an observed longest shard, and a predicted-vs-observed comparison for a worked run
