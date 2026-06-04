## Why

`ssd_define_scenario()` already stores a `partition_by` field with documented per-step defaults, but validates it only as "a list or `NULL`". The targets redesign (`TARGETS-DESIGN.md` §5/§1) makes `partition_by` a load-bearing scenario knob: it picks which task-table columns become Hive **path** levels (one shard per path cell) and which remain **inner** Parquet columns, so `Π |path axis|` sets the shard count per step. `task-list-loop-baseline` (#80, as expanded by `task-list-loop-baseline-fold`) derives the three per-step task tables and already mints a path-style `<step>_id` over *all* of a step's `task_axes()`; `partition_by` generalises that to a configurable subset. The knob needs a real contract — a validated named list with per-step axis vocabularies and a defined path-vs-inner split — before `task-tables`/`hive-partitioning` consume it. Prerequisites (`ssd-define-scenario`, `task-list-loop-baseline` #80 + `task-list-loop-baseline-fold`) have landed.

## What Changes

- Turn `partition_by` into a fully validated, configurable scenario knob. When supplied it SHALL be a named list with **`sample`, `fit`, and `hc`** entries — one per step (`task-list-loop-baseline-fold` folds the `data` truncation into `fit`) — each a character vector of path-axis names drawn from that step's vocabulary.
- Reuse `task_axes(step)` as the per-step axis vocabulary (the single source of truth; no duplicated constant): `sample` = `dataset`, `sim`, `replace`; `fit` adds `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `ci`, `nboot`, `est_method`, `ci_method`, `parametric`. `nrow` is rejected **only** for the `sample` step (the shared draw has no `nrow` axis); it is a valid path axis for `fit`/`hc`, since `fit` truncates its sample inline (`head`, RNG-free) so `nrow` is a genuine `fit` cross-join axis.
- Validate, in the user-facing frame, that each entry's names are unique, non-missing, and a subset of the step vocabulary; abort with an informative error naming the offending step/axis otherwise. Validation is **per-step only** — there is deliberately **no cross-step parent-consistency constraint**. Steps partition independently; a child shard may span several parent shards (an **m:n** relationship), which is resolved at the read layer (`shard-runner-baseline`/`task-tables`) by reading the set of parent shards a child needs and filtering, not by restricting `partition_by`.
- Define the **inner (Parquet) axes** as `setdiff(task_axes(step), path)` and expose the split via an internal accessor `scenario_partition_axes(scenario, step)` that `task-tables`/`hive-partitioning` consume; the path axes drive the `<step>_id` `path_key()`. Use the documented per-step defaults as the fallback when `partition_by` is `NULL`.
- Render `partition_by` (path axes per step) in `print.ssdsims_scenario()`.

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability. -->

### Modified Capabilities
- `scenario-definition`: the `partition_by` field gains a real contract — a validated, configurable named list of per-step path axes with a defined inner-axis complement — beyond the existing "list-or-`NULL`, defaults populated" behaviour.

## Impact

- **New code**: `partition_by` validation in `R/scenario.R` (reusing `task_axes()` from `R/task-lists.R` as the vocabulary source); an internal `scenario_partition_axes(scenario, step)` accessor returning `list(path = ..., inner = ...)`; the three-step defaults; print-path update; tests in `tests/testthat/test-scenario.R`.
- **APIs**: No new exports. `ssd_define_scenario()` gains the three-step (`sample`/`fit`/`hc`) `partition_by` contract and rejects malformed input. Roxygen/`man/` updates documenting the path-vs-inner semantics and the per-step vocabularies.
- **Dependencies**: None added. Builds on `task-list-loop-baseline` (#80) and its `task-list-loop-baseline-fold` expansion: reuses `task_axes()` and aligns with the `sample ← fit ← hc` `<step>_id`/`<parent>_id` path-key linkage.
- **Downstream**: Unblocks `task-tables` and `hive-partitioning` (§12), which let #80's `add_task_ids()`/`path_key()` key on the chosen path-axis subset (instead of all axes) and carry the inner axes as Parquet columns. Also feeds `shard-completeness-assert` (§6.2/§8.4): a shard's `expected_rows` is derived from the **inner** axes and stored by `task-tables`. The split is **orthogonal to the per-task primer** (which hashes all of `task_axes(step)`), so the shard-level acceptance test (changing `partition_by` shifts file paths while per-task results stay byte-identical) holds by construction; it lands with `hive-partitioning`, once shards exist.
