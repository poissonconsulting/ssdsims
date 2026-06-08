## MODIFIED Requirements

### Requirement: The data-step-vs-fold decision is settled to keep the fold
Under the pinned content-hash model the deferred `TARGETS-DESIGN.md` §8 decision SHALL be resolved by **keeping the `fit`-inline `head(sample, nrow)` fold** (`task-list-loop-baseline-fold`): the pipeline SHALL NOT reinstate a materialised `data` step. Because a `fit` shard is keyed by `fit_id` (which includes `nrow`), extending `nrow` SHALL mint new `fit` shards and leave existing ones cached. The shared `sample` draw size is the scenario's fixed `nrow_max` setting (resolved per dataset), **not** `max(nrow)`, so extending `nrow` (within the effective draw size) SHALL NOT change the `sample` shard — the previously-feared "growing `n_max` leaves a stale short `sample` draw" hazard cannot arise, because the draw size no longer grows with `nrow`. The `sample` shard rebuilds only when an input within its slice changes (e.g. `nrow_max` or the dataset), not when `nrow` grows.

#### Scenario: Extending nrow leaves the sample shard cached
- **WHEN** a scenario is run, then re-run after adding an `nrow` value no greater than the effective draw size, and `tar_make()` is run again
- **THEN** the `sample` shard SHALL be skipped (cached) because its fixed `nrow_max` draw is unchanged, and only the new `nrow`-keyed `fit` shards (and downstream `hc`/`summary`) SHALL build — no `fit` shard SHALL read a stale shorter `sample` draw

#### Scenario: Changing nrow_max rebuilds the sample draw
- **WHEN** a scenario is run to completion, `nrow_max` is then changed, and `tar_make()` is run again
- **THEN** the `sample` shards SHALL rebuild with the new draw size (the change is within the `sample` slice), and the dependent `fit`/`hc`/`summary` shards SHALL rebuild through the per-child edge

#### Scenario: No materialised data step is introduced
- **WHEN** the pipeline factory builds the step targets for a scenario
- **THEN** the steps SHALL remain `sample` / `fit` / `hc` with the `head(sample, nrow)` truncation folded into `fit`, and no separate materialised `data` shard target SHALL be created

### Requirement: Each step target depends only on its minimal scenario slice
The `ssd_scenario_targets()` factory SHALL make each step's `tar_map()` command depend on only the **minimal slice** of the scenario that step's per-shard runner consumes — the resolved per-step inputs and the fields reaching that step's per-task body and its `shard_path()`/`read_parent_shards()` primer — rather than the bare `scenario` global. A step target's `targets` dependency hash SHALL therefore cover only its slice, so editing a scenario field outside a step's slice SHALL leave that step's shards cached. The package SHALL provide a deterministic, hashable `scenario_step_slice(scenario, step, datasets)` helper that returns this slice (preserving the `ssdsims_scenario` class so the runners' input contract is unchanged): `sample` consumes the datasets, `nrow_max`, and `partition_by$sample`; `fit` consumes `fit$dists`, the `min_pmix` functions, and `partition_by` for `sample` and `fit`; `hc` consumes `hc$proportion`, `hc$samples`, `hc$ci`, and `partition_by` for `fit` and `hc`. Because the `sample` slice carries datasets, the factory SHALL build it **per shard**, carrying only the dataset(s) that shard reads (its `unique(tasks$dataset)`) as a per-shard mapped value, so a sample shard depends on no dataset it does not draw from. The per-task results SHALL be unchanged (byte-identical to `ssd_run_scenario_baseline()`), because the slice carries exactly the fields the runner reads. The precise expected-cached set SHALL follow the invalidation model pinned by `hive-partitioning` (`TARGETS-DESIGN.md` §8).

#### Scenario: Changing an hc-only knob rebuilds only hc and summary
- **WHEN** a scenario is run to completion with `tar_make()`, an `hc`-only knob (e.g. `hc$samples` or `hc$ci`) is then changed, and `tar_make()` is run again
- **THEN** only the `hc` shards (and `summary`) SHALL rebuild, while every `sample` and `fit` shard SHALL be skipped (cached)

#### Scenario: Changing a fit-only knob leaves sample cached
- **WHEN** a scenario is run to completion with `tar_make()`, a `fit`-only knob (e.g. `fit$dists`) is then changed, and `tar_make()` is run again
- **THEN** the `sample` shards SHALL be skipped (cached), while the `fit` (and downstream `hc`/`summary`) shards SHALL rebuild

#### Scenario: Appending a dataset caches every existing shard
- **WHEN** a scenario is run to completion with `tar_make()`, a new dataset is appended (a path-axis growth that holds `partition_by` fixed), and the pipeline is re-sourced
- **THEN** only the new dataset's `sample`/`fit`/`hc` shards (and `summary`) SHALL build, while every pre-existing dataset's `sample`/`fit`/`hc` shard SHALL be skipped (cached) — the per-shard `sample` slice keeps each existing shard's command byte-identical when the dataset set grows

#### Scenario: A step's slice carries only the fields its runner reads
- **WHEN** `scenario_step_slice(scenario, step, datasets)` is computed for each of `sample`, `fit`, and `hc`
- **THEN** each slice SHALL be a deterministic, hashable `ssdsims_scenario`-classed object carrying exactly the fields that step's runner consumes (the named `datasets` + `nrow_max` / `fit` grid + `min_pmix` functions / `hc` knobs incl. `ci`, plus the step's own and parent `partition_by` axes) and SHALL omit fields no other-step runner reads, so re-sourcing the same scenario yields a byte-identical slice
