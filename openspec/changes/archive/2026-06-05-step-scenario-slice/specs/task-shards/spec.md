## ADDED Requirements

### Requirement: Each step target depends only on its minimal scenario slice
The `ssd_scenario_targets()` factory SHALL make each step's `tar_map()` command depend on only the **minimal slice** of the scenario that step's per-shard runner consumes — the resolved per-step inputs and the fields reaching that step's per-task body and its `shard_path()`/`read_parent_shards()` primer — rather than the bare `scenario` global. A step target's `targets` dependency hash SHALL therefore cover only its slice, so editing a scenario field outside a step's slice SHALL leave that step's shards cached. The package SHALL provide a deterministic, hashable `scenario_step_slice(scenario, step, datasets)` helper that returns this slice (preserving the `ssdsims_scenario` class so the runners' input contract is unchanged): `sample` consumes the datasets and `partition_by$sample`; `fit` consumes `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; `hc` consumes `hc$proportion`, `hc$samples`, and `partition_by` for `fit` and `hc`. Because the `sample` slice carries datasets, the factory SHALL build it **per shard**, carrying only the dataset(s) that shard reads (its `unique(tasks$dataset)`) as a per-shard mapped value, so a sample shard depends on no dataset it does not draw from. The per-task results SHALL be unchanged (byte-identical to `ssd_run_scenario_baseline()`), because the slice carries exactly the fields the runner reads. The precise expected-cached set SHALL follow the invalidation model pinned by `hive-partitioning` (`TARGETS-DESIGN.md` §8); this requirement is finalised once that decision lands.

#### Scenario: Changing an hc-only scenario option rebuilds only hc and summary
- **WHEN** a scenario is run to completion with `tar_make()`, an `hc`-only scenario option (e.g. `hc$samples`) is then changed, and `tar_make()` is run again
- **THEN** only the `hc` shards (and `summary`) SHALL rebuild, while every `sample` and `fit` shard SHALL be skipped (cached)

#### Scenario: Changing a fit-only scenario option leaves sample cached
- **WHEN** a scenario is run to completion with `tar_make()`, a `fit`-only scenario option (e.g. `fit$dists`) is then changed, and `tar_make()` is run again
- **THEN** the `sample` shards SHALL be skipped (cached), while the `fit` (and downstream `hc`/`summary`) shards SHALL rebuild

#### Scenario: Appending a dataset caches every existing shard
- **WHEN** a scenario is run to completion with `tar_make()`, a new dataset is appended (a path-axis growth that holds `partition_by` fixed), and the pipeline is re-sourced
- **THEN** only the new dataset's `sample`/`fit`/`hc` shards (and `summary`) SHALL build, while every pre-existing dataset's `sample`/`fit`/`hc` shard SHALL be skipped (cached) — the per-shard `sample` slice keeps each existing shard's command byte-identical when the dataset set grows

#### Scenario: A step's slice carries only the fields its runner reads
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed for each of `sample`, `fit`, and `hc`
- **THEN** each slice SHALL be a deterministic, hashable `ssdsims_scenario`-classed object carrying exactly the fields that step's runner consumes (the named `datasets` / `fit` grid + `min_pmix` functions / `hc` scenario options, plus the step's own and parent `partition_by` axes) and SHALL omit fields no other-step runner reads, so re-sourcing the same scenario yields a byte-identical slice
