## ADDED Requirements

### Requirement: Isolate a distribution set by name from a scenario
The package SHALL provide `scenario_distset(scenario, name)` that returns the
member character vector of the distribution set stored on the scenario under
`name` (from `scenario$hc$distsets`), and SHALL abort with an informative error
when `name` is not one of the scenario's distribution-set names. Like the other
accessors it SHALL resolve by name without registration, persistence, or
re-validation — the set members were validated at construction
(`ssd_define_scenario()`), and the hc runner uses this accessor to resolve a
task's `distset` name back to the members it subsets the union fit by.

#### Scenario: Returns the member vector for a known set name
- **WHEN** `scenario_distset(scenario, name)` is called for a `name` in the scenario's distribution-set names
- **THEN** it SHALL return that set's member character vector

#### Scenario: Unknown distset name errors
- **WHEN** `scenario_distset(scenario, name)` is called for a `name` not in the scenario's distribution-set names
- **THEN** it SHALL abort with an informative error naming the unknown set

## MODIFIED Requirements

### Requirement: Isolate a step's minimal scenario slice
The package SHALL provide an internal `scenario_step_slice(scenario, step, datasets)` that returns the minimal sub-scenario the named step's per-shard runner consumes, keeping the `ssdsims_scenario` class so the existing accessors (`scenario_dataset()`, `scenario_min_pmix()`, `scenario_distset()`) and the runners' `chk_s3_class()` contract work unchanged on the slice. Like the other accessors it SHALL resolve by name/structure without registration, persistence, or re-validation, and it SHALL be a **pure, deterministic** function of its arguments (no environment capture), so the slice is hashable and byte-identical across re-sourcings of the same scenario. The slice SHALL preserve name-only task hashing: it carries the `min_pmix` *functions* (which hash by name, not body) for `fit`, and the **distribution-set member vectors** (which hash by set name) for `hc`, so neither a function-body edit nor a set's membership riding for execution SHALL move a cached shard through the slice. The `datasets` argument SHALL restrict the `sample` slice's carried datasets to the named subset (those a *shard* reads, its `unique(tasks$dataset)`), defaulting to all of the scenario's datasets; it SHALL NOT affect the `fit`/`hc` slices, which carry no datasets.

#### Scenario: Each step's slice carries its runner's inputs
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)`, `scenario_step_slice(scenario, "fit")`, and `scenario_step_slice(scenario, "hc")` are computed
- **THEN** the `sample` slice SHALL carry the named `datasets` and `partition_by$sample`; the `fit` slice SHALL carry `fit$dists` (the union), the `min_pmix` functions, and `partition_by` for `sample` and `fit`; and the `hc` slice SHALL carry `hc$proportion`, `hc$samples`, the `hc$distsets` member vectors, and `partition_by` for `fit` and `hc`

#### Scenario: The sample slice carries only the named datasets
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)` is computed for a subset of the scenario's datasets, and again for a scenario that additionally defines further datasets
- **THEN** both slices SHALL be byte-identical (carrying only the named `datasets`), so a sample shard's slice is independent of the datasets it does not read

#### Scenario: The slice is deterministic and hashable
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed twice for the same arguments
- **THEN** the two slices SHALL be byte-identical, so they produce the same dependency hash
