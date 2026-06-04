## Why

The in-memory baseline runner (`ssd_run_scenario_baseline()`, `task-list-loop-baseline` + `…-fold`, now seeded by `primer-primitives`) threads each step's results forward as **in-memory named lists keyed by `<step>_id`** — no shards, no Parquet, no `partition_by`. The targets pipeline (`TARGETS-DESIGN.md` §5/§6) instead stores each step as **Hive-partitioned Parquet shards** and reads parents back with duckplyr predicate pushdown. That write→glob-read→filter loop — and the **m:n parent-shard dependency** it implies under the §5 coarsening defaults (an `hc` shard reads several `fit` shards; a `fit` shard reads several `sample` shards) — is currently unproven anywhere in plain R; it is entangled with `targets` static branching in `task-tables`/`hive-partitioning`. This change lands a **single-core runner over partitioned shards** as the missing middle rung: "shards without `targets`", mirroring how `task-list-loop-baseline` was "tasks without shards". It is the first concrete consumer of `partition-by`'s `scenario_partition_axes()` and the place where m:n is implemented and tested — de-risking the `targets`-mechanics steps that follow.

## What Changes

- Add a single-core sharded runner (working name `ssd_run_scenario_shards(scenario, dir)`) that runs the three steps in order (`sample` → `fit` → `hc`) using the **existing** `*_data_task_primer()` wrappers, so per-task RNG and reproducibility are **unchanged** (`partition_by` is orthogonal to the primer — re-layout never changes results).
- For each step, **group tasks into shards** by `scenario_partition_axes(scenario, step)$path` (from `partition-by`), run each shard's tasks via the per-task primer loop, and **write one Parquet per shard** under a Hive path `<dir>/<step>/<path-axis=value>/.../part.parquet`, carrying the step's **inner** axes and the per-task results as columns.
- For each non-root step, **read parent shards back via duckplyr** (`duckplyr::read_parquet_duckdb()` over the parent's Hive glob) and **filter** to the rows each child shard needs — predicate pushdown on the parent's path/inner columns.
- **Own the m:n parent-shard dependency resolution**: given a child shard, compute the **distinct set of parent shard paths** over its tasks (the `<parent>_id` full-identity foreign key projected onto `path[parent]`) and read+filter exactly those. No assumption of one parent shard per child shard.
- Single core, in-process: a plain loop over shards. **No `targets`, no `crew`, no cloud upload.** The runner is order-independent (shards may run in any order for a fixed `scenario$seed`).

## Capabilities

### New Capabilities
- `shard-runner`: a single-core runner that materialises each step's results as Hive-partitioned Parquet shards (one Parquet per `partition_by` path cell) and links steps by reading parent shards back via duckplyr with predicate pushdown, resolving the m:n child-shard ← parent-shards dependency at run time. Per-task RNG, results, and reproducibility match the in-memory baseline runner exactly; only the storage layout differs.

### Modified Capabilities
<!-- None. The in-memory `task-lists` baseline runner is unchanged and serves as the byte-identical reference oracle; `scenario-definition`'s `scenario_partition_axes()` is consumed, not modified. -->

## Impact

- **New code**: the sharded runner and its helpers (shard grouping by `scenario_partition_axes()`, Hive path rendering reusing `path_key()`, per-shard Parquet write, parent-shard glob-read + filter, m:n parent-set resolution) in a new `R/shard-runner.R`; tests in `tests/testthat/test-shard-runner.R`.
- **APIs**: one new export (working name `ssd_run_scenario_shards()`); `NAMESPACE`/`man/` entries. The in-memory `ssd_run_scenario_baseline()` is untouched and used as the reference oracle in tests.
- **Dependencies**: adds **`duckplyr`** (which brings `duckdb`) to `Imports`. *Decision:* the §6 design used duckplyr only for downstream/off-cluster reads; using it for **intermediate inter-step storage** on tiny datasets is heavier than `arrow`, but it is the **same engine the targets read path commits to**, so proving the loop single-core directly de-risks `hive-partitioning`. We lean duckplyr for that consistency (see design.md).
- **Dependencies (changes)**: builds on `primer-primitives` (the `*_data_task_primer()` wrappers + per-task reproducibility) and `partition-by` (`scenario_partition_axes()` path/inner split). Adds a `prims → shard-runner-baseline` and `partby → shard-runner-baseline` edge to the §12 DAG; sibling to `task-tables`.
- **Downstream**: de-risks `hive-partitioning`/`task-tables`/`cluster-pipeline` by validating the write→glob-read→filter loop and m:n resolution in plain R. Makes `partition-by`'s **deferred** acceptance test ("changing `partition_by` shifts file paths while per-task results stay byte-identical") landable **without `targets`**.
- **Out of scope / deferred (flagged, not decided here)**: how the eventual `targets` static-branching layer wires m:n fan-in edges. The lean — recorded for `task-tables` — is **Option 3** (compute each child shard's upstream target-name set at sourcing time and splice it in), which preserves both m:n and fine-grained invalidation and keeps the parent-consistency rule permanently absent from `partition-by`. The `partition-by` cleanup (removing the parent-consistency requirement) and the latent `baseline → partby` DAG edge are tracked separately, not in this change's tasks.
