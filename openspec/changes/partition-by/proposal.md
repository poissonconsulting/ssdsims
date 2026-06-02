## Why

`ssd_define_scenario()` already stores a `partition_by` field with documented per-step defaults, but validates it only as "a list or `NULL`". The targets redesign (`TARGETS-DESIGN.md` §5/§1) makes `partition_by` a load-bearing scenario knob: it picks which task-table columns become Hive **path** levels (one shard per path cell) and which remain **inner** Parquet columns, so `Π |path axis|` sets the shard count per step. `task-list-loop-baseline` (#80) now derives the four per-step task tables and already mints a path-style `<step>_id` over *all* of a step's `task_axes()`; `partition_by` generalises that to a configurable subset. The knob needs a real contract — a validated named list with per-step axis vocabularies and a defined path-vs-inner split — before `task-tables`/`hive-partitioning` consume it. Prerequisites (`ssd-define-scenario`, `task-list-loop-baseline` #80) have landed.

## What Changes

- Turn `partition_by` into a fully validated, configurable scenario knob. When supplied it SHALL be a named list with **`sample`, `data`, `fit`, and `hc`** entries — one per #80 step — each a character vector of path-axis names drawn from that step's vocabulary.
- Reuse #80's `task_axes(step)` as the per-step axis vocabulary (the single source of truth; no duplicated constant): `sample` = `dataset`, `sim`, `replace`; `data` adds `nrow`; `fit` adds `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `ci`, `nboot`, `est_method`, `ci_method`, `parametric`. `nrow` is rejected **only** for the `sample` step (the shared draw has no `nrow` axis); it is a valid path axis for `data`/`fit`/`hc`, since the RNG-free truncation step makes `nrow` a genuine cross-join axis (#80).
- Validate, in the user-facing frame, that each entry's names are unique, non-missing, and a subset of the step vocabulary; abort with an informative error naming the offending step/axis otherwise. Also validate parent-consistency: a child step's path axes (restricted to the parent's vocabulary) SHALL be a subset of the parent's path axes, so the `<parent>_id` foreign-key join stays well-defined.
- Define the **inner (Parquet) axes** as `setdiff(task_axes(step), path)` and expose the split via an internal accessor `scenario_partition_axes(scenario, step)` that `task-tables`/`hive-partitioning` consume; the path axes drive #80's `<step>_id` `path_key()`. Use the documented per-step defaults as the fallback when `partition_by` is `NULL`.
- Render `partition_by` (path axes per step) in `print.ssdsims_scenario()`.

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability. -->

### Modified Capabilities
- `scenario-definition`: the `partition_by` field gains a real contract — a validated, configurable named list of per-step path axes with a defined inner-axis complement — beyond the existing "list-or-`NULL`, defaults populated" behaviour.

## Impact

- **New code**: `partition_by` validation in `R/scenario.R` (reusing #80's `task_axes()` from `R/task-lists.R` as the vocabulary source); an internal `scenario_partition_axes(scenario, step)` accessor returning `list(path = ..., inner = ...)`; the four-step defaults; print-path update; tests in `tests/testthat/test-scenario.R`.
- **APIs**: No new exports. `ssd_define_scenario()` gains the four-step (`sample`/`data`/`fit`/`hc`) `partition_by` contract and rejects malformed input. Roxygen/`man/` updates documenting the path-vs-inner semantics and the per-step vocabularies.
- **Dependencies**: None added. Builds on #80 (`task-list-loop-baseline`): reuses `task_axes()` and aligns with the `<step>_id`/`<parent>_id` path-key linkage.
- **Downstream**: Unblocks `task-tables` and `hive-partitioning` (§12), which let #80's `add_task_ids()`/`path_key()` key on the chosen path-axis subset (instead of all axes) and carry the inner axes as Parquet columns. The shard-level acceptance test (changing `partition_by` shifts file paths while per-task results stay byte-identical) lands with `hive-partitioning`, once shards exist.
