## Why

`ssd_define_scenario()` already stores a `partition_by` field with documented per-step defaults, but validates it only as "a list or `NULL`". The targets redesign (`TARGETS-DESIGN.md` §5/§1) makes `partition_by` a load-bearing scenario knob: it picks which task-table columns become Hive **path** levels (one shard per path cell) and which remain **inner** Parquet columns, so `Π |path axis|` sets the shard count per step. Before `task-tables`/`hive-partitioning` consume it, the knob needs a real contract — a validated named list with per-step axis vocabularies and a defined path-vs-inner split. Its only prerequisite (`ssd-define-scenario`) has landed; this is a parallel stream off it (roadmap entry `partition-by`).

## What Changes

- Turn `partition_by` into a fully validated, configurable scenario knob. When supplied it SHALL be a named list with `data`, `fit`, and `hc` entries, each a character vector of path-axis names drawn from that step's known axis vocabulary (§5).
- Define the per-step axis vocabulary as the documented set of cross-join axes for each step (data: `dataset`, `sim`, `replace`; fit adds `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; hc adds `nboot`, `est_method`, `ci_method`, `parametric`). `nrow` is **never** an axis (it is sub-truncation, §5) and SHALL be rejected as a path axis.
  - **Depends on `task-list-loop-baseline` (#80).** These axis names mirror the task-table columns that #80 establishes; the vocabulary constant SHALL be reconciled to #80's exact column names when it merges (the vocabulary is the upstream source; the task tables assert agreement downstream). Until then the names above are provisional.
- Validate, in the user-facing frame, that each entry's names are unique, non-missing, and a subset of the step vocabulary; abort with an informative error naming the offending step/axis otherwise.
- Define the **inner (Parquet) axes** as a step's vocabulary minus its path axes, and expose this split via an internal accessor that `task-tables`/`hive-partitioning` will consume. Keep the documented §5 defaults as the fallback when `partition_by` is `NULL`.
  - **Depends on `task-list-loop-baseline` (#80).** Task tables are linked upstream→downstream by ID columns (`data_id`/`fit_id`), which #80 establishes; a step's path axes are the partition columns those shard IDs key on. The accessor's contract SHALL be reconciled with #80's task tables and their ID linkage when it merges, and the `tasks.md` breakdown is provisional and may need revision once #80 lands.
- Render `partition_by` (path axes per step) in `print.ssdsims_scenario()`.

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability. -->

### Modified Capabilities
- `scenario-definition`: the `partition_by` field gains a real contract — a validated, configurable named list of per-step path axes with a defined inner-axis complement — beyond the existing "list-or-`NULL`, defaults populated" behaviour.

## Impact

- **New code**: `partition_by` validation and the per-step axis-vocabulary constant in `R/scenario.R`; an internal `scenario_partition_axes(scenario, step)` accessor returning `list(path = ..., inner = ...)`; print-path update; tests in `tests/testthat/test-scenario.R`.
- **APIs**: No new exports. `ssd_define_scenario()` rejects malformed `partition_by`; the stored field and defaults are unchanged in shape. Roxygen/`man/` updates documenting the path-vs-inner semantics and the per-step vocabularies.
- **Dependencies**: None added.
- **Downstream**: Unblocks `task-tables` and `hive-partitioning` (§12), which turn path axes into shard directory levels and inner axes into Parquet columns. The shard-level acceptance test (changing `partition_by` shifts file paths while per-task results stay byte-identical) lands with `hive-partitioning`, once shards exist.
