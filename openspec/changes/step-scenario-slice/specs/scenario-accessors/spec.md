## ADDED Requirements

### Requirement: Isolate a step's minimal scenario slice
The package SHALL provide an internal `scenario_step_slice(scenario, step)` that returns the minimal sub-scenario the named step's per-shard runner consumes, keeping the `ssdsims_scenario` class so the existing accessors (`scenario_dataset()`, `scenario_min_pmix()`) and the runners' `chk_s3_class()` contract work unchanged on the slice. Like the other accessors it SHALL resolve by name/structure without registration, persistence, or re-validation, and it SHALL be a **pure, deterministic** function of the scenario (no environment capture), so the slice is hashable and byte-identical across re-sourcings of the same scenario. The slice SHALL preserve name-only task hashing: it carries the `min_pmix` *functions* (which hash by name, not body) for `fit`, so a function-body edit SHALL NOT move a cached `fit` shard through the slice.

#### Scenario: Each step's slice carries its runner's inputs
- **WHEN** `scenario_step_slice(scenario, "sample")`, `scenario_step_slice(scenario, "fit")`, and `scenario_step_slice(scenario, "hc")` are computed
- **THEN** the `sample` slice SHALL carry the datasets and `partition_by$sample`; the `fit` slice SHALL carry `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; and the `hc` slice SHALL carry `hc$proportion`, `hc$samples`, and `partition_by` for `fit` and `hc`

#### Scenario: The slice is deterministic and hashable
- **WHEN** `scenario_step_slice(scenario, step)` is computed twice for the same scenario and step
- **THEN** the two slices SHALL be byte-identical, so they produce the same dependency hash
