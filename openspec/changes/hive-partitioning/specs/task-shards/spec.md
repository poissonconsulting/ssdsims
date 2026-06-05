## ADDED Requirements

### Requirement: The shard invalidation model is pinned to content-hash over format = "file"
The shard step targets (`sample_step` / `fit_step` / `hc_step`, one per `partition_by` path cell) SHALL use **content-hash invalidation over their `format = "file"` Parquet outputs** as their pinned invalidation model (`TARGETS-DESIGN.md` §8), read observably as **cache-by-existence**: a shard SHALL be treated as up to date if and only if its output Parquet exists *and* the inputs its body depends on — its task-row set, the scenario fields it reads, and the parent shard target(s) it reads — are unchanged. A shard whose Parquet is missing SHALL rebuild; a shard whose inputs changed SHALL rebuild; a recomputed shard whose Parquet bytes are byte-identical to the prior write SHALL leave its dependents skipped (value-propagation). This model SHALL be a stated property of the pipeline factory, NOT an emergent `targets` default, so the downstream `path-axis-growth`, `shard-atomic-rewrite`, and `step-scenario-slice` changes finalise their expected cached-vs-rebuilt assertions against it.

#### Scenario: A shard with an existing, unchanged Parquet is a cache hit
- **WHEN** a scenario is run through the targets pipeline and `tar_make()` is run a second time with no inputs changed
- **THEN** every shard target SHALL be reported up to date and SHALL NOT be rebuilt, and no shard Parquet SHALL be rewritten

#### Scenario: A shard whose output Parquet is missing rebuilds
- **WHEN** a shard's `format = "file"` Parquet is deleted and `tar_make()` is run again
- **THEN** that shard target SHALL be rebuilt and its Parquet rewritten, while shards whose Parquets still exist and whose inputs are unchanged SHALL stay cached

### Requirement: Per-child upstream edges drive m:n fan-in invalidation
The pipeline factory SHALL wire each child shard target to the **specific parent shard target(s) its tasks read** (the Option-3 per-child upstream edges of `TARGETS-DESIGN.md` §6), computed at sourcing time as the distinct set of parent path cells the child shard's tasks project onto (`unique(path_key(tasks, partition_by[[parent]]))` — the same projection `read_parent_shards()` uses to read them). These per-child edges SHALL **replace** the coarse step-wide `tarchetypes::tar_combine()` barriers (`sample_done` / `fit_done`) as the **invalidation** mechanism between steps, so that rewriting one parent shard invalidates only the child shards that read it, NOT the whole downstream step. The set of parent shard targets a child names SHALL equal the set of parent shards `read_parent_shards()` opens for that child (one source of truth), so the dependency graph and the read are consistent. The portable partition-path read contract (the runner opening parent Parquets by path) SHALL remain unchanged.

#### Scenario: Rewriting one parent shard re-runs only the child shards that read it
- **WHEN** one `sample` shard's Parquet is rewritten with changed bytes (or invalidated) and `tar_make()` is run again
- **THEN** only the `fit` shards whose tasks read that `sample` shard SHALL be re-run, the `fit` shards that read other `sample` shards SHALL stay cached, and the same precise propagation SHALL hold for `fit → hc`

#### Scenario: A child shard names exactly the parents it reads
- **WHEN** the pipeline is sourced for a scenario whose `partition_by` makes a child shard span several parent shards (the m:n case)
- **THEN** that child shard target SHALL declare a dependency edge to each parent shard target it reads, and SHALL declare no edge to a parent shard it does not read

#### Scenario: Per-child edges replace the coarse step barrier
- **WHEN** the factory builds the step targets
- **THEN** the inter-step invalidation SHALL route through the per-child parent-shard edges rather than a step-wide `tar_combine()` barrier, so a single parent-shard change SHALL NOT mark the entire downstream step out of date

### Requirement: Code-pin and forced-refresh semantics against the pinned model
The pinned model SHALL define the `tar_cue()` pin and forced-refresh semantics the §8.3/§8.4 workflows rely on. `tar_cue(depend = FALSE)` on a shard step target SHALL pin it against upstream **dependency/code** changes (an edited per-task `_state`/primitive function or a bumped `ssdtools` version) so trusted shards are not rebuilt by such a change — with the carve-outs that the target SHALL still rebuild if its own `format = "file"` Parquet is missing, if its task-table grouping changes (so path-axis and inner-axis growth still apply under the pin), or if it previously errored (a `error = "null"` short/failed shard retries). `targets::tar_invalidate(names = ...)` (or deleting the shard's Parquet) SHALL force a refresh of the named shards, overriding the pin, so the mixed-code case (keep trusted shards, recompute a chosen few) is expressible against the pinned model.

#### Scenario: A code edit does not rebuild pinned shards
- **WHEN** a shard step target carries `tar_cue(depend = FALSE)`, a per-task primitive function it calls is edited, and `tar_make()` is run again
- **THEN** the pinned shards SHALL NOT be rebuilt, while a shard whose Parquet is missing or whose task-table grouping changed SHALL still rebuild

#### Scenario: Forced refresh overrides the pin for chosen shards
- **WHEN** `tar_invalidate()` is called on a chosen set of pinned shards (or their Parquets are deleted) and `tar_make()` is run again
- **THEN** exactly those shards SHALL be recomputed under the current code while the other pinned shards SHALL stay cached

### Requirement: The data-step-vs-fold decision is settled to keep the fold
Under the pinned content-hash model the deferred `TARGETS-DESIGN.md` §8 decision SHALL be resolved by **keeping the `fit`-inline `head(sample, nrow)` fold** (`task-list-loop-baseline-fold`): the pipeline SHALL NOT reinstate a materialised `data` step. Because a `fit` shard is keyed by `fit_id` (which includes `nrow`), extending `nrow` SHALL mint new `fit` shards and leave existing ones cached, so the fold is sufficient and a `data` checkpoint is redundant. The dual hazard that growing `n_max` (extending `nrow` upward) could leave a stale short `sample` draw SHALL be handled by the model rather than a `data` step: a widened `n_max` changes the `sample` shard's task row, so the `sample` shard's bytes change and the per-child upstream edge propagates that change to exactly the `fit` shards that read the widened draw.

#### Scenario: Extending nrow does not produce a stale sample draw
- **WHEN** a scenario is run, then re-run after widening `max(nrow)` so the `sample` draw's `n_max` grows, and `tar_make()` is run again
- **THEN** the affected `sample` shard SHALL be rewritten with the wider draw, the `fit` shards that read it SHALL be invalidated through the per-child edge, and no `fit` shard SHALL read a stale shorter `sample` draw

#### Scenario: No materialised data step is introduced
- **WHEN** the pipeline factory builds the step targets for a scenario
- **THEN** the steps SHALL remain `sample` / `fit` / `hc` with the `head(sample, nrow)` truncation folded into `fit`, and no separate materialised `data` shard target SHALL be created
