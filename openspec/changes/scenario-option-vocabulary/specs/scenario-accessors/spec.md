# scenario-accessors Delta

## MODIFIED Requirements

### Requirement: Isolate a step's minimal scenario slice
The package SHALL provide an internal `scenario_step_slice(scenario, step, datasets)` that returns the minimal sub-scenario the named step's per-shard runner consumes, keeping the `ssdsims_scenario` class so the existing accessors (`scenario_dataset()`, `scenario_min_pmix()`) and the runners' `chk_s3_class()` contract work unchanged on the slice. Like the other accessors it SHALL resolve by name/structure without registration, persistence, or re-validation, and it SHALL be a **pure, deterministic** function of its arguments (no environment capture), so the slice is hashable and byte-identical across re-sourcings of the same scenario. The slice SHALL preserve name-only task hashing: it carries the `min_pmix` *functions* (which hash by name, not body) for `fit`, so a function-body edit SHALL NOT move a cached `fit` shard through the slice. The `datasets` argument SHALL restrict the `sample` slice's carried datasets to the named subset (those a *shard* reads, its `unique(tasks$dataset)`), defaulting to all of the scenario's datasets; it SHALL NOT affect the `fit`/`hc` slices, which carry no datasets. The slice SHALL carry the non-axis scenario settings each step's runner reads: the `sample` slice carries `nrow_max` (the draw-size setting the `sample` runner resolves against each dataset), and the `hc` slice carries `ci` (read by the `hc` runner) alongside `proportion` and `samples`.

#### Scenario: Each step's slice carries its runner's inputs
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)`, `scenario_step_slice(scenario, "fit")`, and `scenario_step_slice(scenario, "hc")` are computed
- **THEN** the `sample` slice SHALL carry the named `datasets`, `nrow_max`, and `partition_by$sample`; the `fit` slice SHALL carry `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; and the `hc` slice SHALL carry `hc$proportion`, `hc$samples`, `hc$ci`, and `partition_by` for `fit` and `hc`

#### Scenario: The sample slice carries only the named datasets
- **WHEN** `scenario_step_slice(scenario, "sample", datasets)` is computed for a subset of the scenario's datasets, and again for a scenario that additionally defines further datasets
- **THEN** both slices SHALL be byte-identical (carrying only the named `datasets` and `nrow_max`), so a sample shard's slice is independent of the datasets it does not read

#### Scenario: The slice is deterministic and hashable
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed twice for the same arguments
- **THEN** the two slices SHALL be byte-identical, so they produce the same dependency hash
