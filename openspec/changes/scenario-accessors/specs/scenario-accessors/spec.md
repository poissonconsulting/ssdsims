## ADDED Requirements

### Requirement: Materialise min_pmix functions on the scenario
`ssd_define_scenario()` SHALL store, alongside each `min_pmix` *name* it already records, the resolved single-argument **function** for that name, keyed by name. A supplied function SHALL be stored under its derived name; a name-string reference SHALL be resolved to a function at construction (from `ssdtools` or the caller's environment) and stored, and the constructor SHALL abort with an informative error if a referenced name cannot be resolved to a single-argument function. The stored `min_pmix` names — and therefore task hashes — SHALL be unchanged by this materialisation.

#### Scenario: A supplied min_pmix function is materialised under its name
- **WHEN** `ssd_define_scenario(..., min_pmix = my_fun)` is called with a single-argument function
- **THEN** the scenario SHALL store `my_fun` keyed by its derived name, and the `min_pmix` name recorded for hashing SHALL be that same derived name

#### Scenario: A min_pmix name-string is resolved at construction
- **WHEN** `ssd_define_scenario(..., min_pmix = "ssd_min_pmix")` is called
- **THEN** the scenario SHALL resolve `"ssd_min_pmix"` to its function at construction and store it keyed by that name

#### Scenario: An unresolvable min_pmix name fails fast
- **WHEN** `ssd_define_scenario(..., min_pmix = "no_such_fun")` is called and the name resolves to no single-argument function
- **THEN** the constructor SHALL abort with an informative error naming the offending entry, in the `ssd_define_scenario()` frame

#### Scenario: Materialisation does not change task hashes
- **WHEN** two scenarios are built with the same `min_pmix` name but functions whose bodies differ
- **THEN** the `min_pmix` name stored for hashing SHALL be identical for both, so a fit task's primer SHALL be byte-identical across them

### Requirement: Isolate a dataset by name from a scenario
The package SHALL provide `scenario_dataset(scenario, name)` that returns the materialised dataset tibble stored on the scenario under `name`, and SHALL abort with an informative error when `name` is not one of the scenario's datasets. The accessor SHALL perform no registration, persistence, or re-validation — the dataset was validated and materialised at construction.

#### Scenario: Returns the materialised dataset for a known name
- **WHEN** `scenario_dataset(scenario, name)` is called for a `name` in the scenario's datasets
- **THEN** it SHALL return that dataset's materialised tibble

#### Scenario: Unknown dataset name errors
- **WHEN** `scenario_dataset(scenario, name)` is called for a `name` not in the scenario's datasets
- **THEN** it SHALL abort with an informative error naming the unknown dataset

### Requirement: Isolate a min_pmix function by name from a scenario
The package SHALL provide `scenario_min_pmix(scenario, name)` that returns the materialised `min_pmix` function stored on the scenario under `name`, and SHALL abort with an informative error when `name` is not one of the scenario's `min_pmix` names.

#### Scenario: Returns the materialised function for a known name
- **WHEN** `scenario_min_pmix(scenario, name)` is called for a `name` in the scenario's `min_pmix` names
- **THEN** it SHALL return the single-argument function materialised for that name

#### Scenario: Unknown min_pmix name errors
- **WHEN** `scenario_min_pmix(scenario, name)` is called for a `name` not in the scenario's `min_pmix` names
- **THEN** it SHALL abort with an informative error naming the unknown entry

### Requirement: min_pmix resolution uses the accessor, not a runtime lookup
`resolve_min_pmix()` SHALL resolve a `min_pmix` name through the scenario accessor (`scenario_min_pmix()`) rather than searching `ssdtools` or the global environment at run time, so that resolution depends only on the scenario and a cluster worker needs no shared interactive environment. For a built-in `min_pmix` (e.g. `"ssd_min_pmix"`), the resolved function SHALL be the same one the previous runtime lookup would have returned, so the baseline runner's results are unchanged.

#### Scenario: Resolution returns the materialised function
- **WHEN** a fit task resolves its `min_pmix` name during a run
- **THEN** it SHALL obtain the function from the scenario (the materialised store), not from a runtime `ssdtools`/global-env search

#### Scenario: Built-in min_pmix is unchanged for the baseline runner
- **WHEN** a scenario using the default `min_pmix` is run through the baseline runner before and after this change
- **THEN** the fit results SHALL be unchanged
