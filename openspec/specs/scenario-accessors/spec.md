# scenario-accessors Specification

## Purpose

Materialise a scenario's name-referenced parameters **on the scenario** and reach them **by name** — no registry, no runtime environment search (`TARGETS-DESIGN.md` §1.1). `ssd_define_scenario()` resolves and stores each `min_pmix` function keyed by name at construction; `scenario_dataset()` and `scenario_min_pmix()` isolate a dataset or `min_pmix` function by name; and `resolve_min_pmix()` resolves through the accessor so a cluster worker needs no shared interactive environment. Hashing stays name-only, so a function-body edit never moves a cached fit branch.

## Requirements

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

### Requirement: Isolate a step's minimal scenario slice
The package SHALL provide an internal `scenario_step_slice(scenario, step, datasets)` that returns the minimal sub-scenario the named step's per-shard runner consumes, keeping the `ssdsims_scenario` class so the existing accessors (`scenario_dataset()`, `scenario_min_pmix()`) and the runners' `chk_s3_class()` contract work unchanged on the slice. Like the other accessors it SHALL resolve by name/structure without registration, persistence, or re-validation, and it SHALL be a **pure, deterministic** function of its arguments (no environment capture), so the slice is hashable and byte-identical across re-sourcings of the same scenario. The slice SHALL preserve name-only task hashing: it carries the `min_pmix` *functions* (which hash by name, not body) for `fit`, so a function-body edit SHALL NOT move a cached `fit` shard through the slice. The `datasets` argument SHALL restrict the `sample` slice's carried datasets to the named subset (those a *shard* reads, its `unique(tasks$dataset)`), defaulting to all of the scenario's datasets; it SHALL NOT affect the `fit`/`hc` slices, which carry no datasets. The slice SHALL carry the non-axis simulation settings each step's runner reads: the `sample` slice carries `nrow_max` (the draw-size setting the `sample` runner resolves against each dataset), and the `hc` slice carries `ci` (read by the `hc` runner) alongside `proportion` and `samples`.

#### Scenario: Each step's slice carries its runner's inputs
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)`, `scenario_step_slice(scenario, "fit")`, and `scenario_step_slice(scenario, "hc")` are computed
- **THEN** the `sample` slice SHALL carry the named `datasets`, `nrow_max`, and `partition_by$sample`; the `fit` slice SHALL carry `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; and the `hc` slice SHALL carry `hc$proportion`, `hc$samples`, `hc$ci`, and `partition_by` for `fit` and `hc`

#### Scenario: The sample slice carries only the named datasets
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)` is computed for a subset of the scenario's datasets, and again for a scenario that additionally defines further datasets
- **THEN** both slices SHALL be byte-identical (carrying only the named `datasets` and `nrow_max`), so a sample shard's slice is independent of the datasets it does not read

#### Scenario: The slice is deterministic and hashable
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed twice for the same arguments
- **THEN** the two slices SHALL be byte-identical, so they produce the same dependency hash
