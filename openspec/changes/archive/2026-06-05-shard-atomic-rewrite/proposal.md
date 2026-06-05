## Why

`TARGETS-DESIGN.md` §8.2 specifies how the pipeline reacts when an *inner* axis grows — an axis that is **not** in a step's `partition_by`, e.g. adding a new `min_pmix` to the scenario's `fit` grid. Such a value does not mint new shards (that is path-axis growth, §8.1); instead it adds one task to **every** affected shard, so each affected shard must be **atomically rewritten** with its larger task set while preserving per-task RNG identity. The mechanism already falls out of static branching plus per-layout roots (`task-tables`/`task-shards`, landed): `targets` sees the grouped task table change, marks those branches stale, the body re-runs all K tasks in each shard and overwrites the Parquet. What does **not** exist is the explicit *contract* — that the rows present before come out byte-identical to the prior Parquet, that the rewrite is whole-file (no append), and that untouched steps stay cached — nor an end-to-end **test** pinning it. Without that, the byte-stability §8.2 leans on (and that §8.4/§9 inherit) is asserted nowhere and could silently regress.

## What Changes

- Add a `task-shards` requirement that pins the §8.2 inner-axis-growth contract: adding an inner-axis value (e.g. a new `min_pmix` for `fit`) marks the affected step's shard branches stale, re-runs each affected shard over its full new task set, and **overwrites** the shard Parquet as a whole (one Parquet per shard, no in-place append).
- Pin the **byte-stability** guarantee: the rows present in a shard before the growth re-emit byte-identical to the prior Parquet (per-task `(seed, primer)` unchanged), the rewritten file differing only by the added task row(s). Call out that this rests on **deterministic Parquet writes** — a fixed column order under a pinned `duckplyr`/DuckDB.
- Pin the **caching** half: shards in steps the growth does not touch (e.g. the `sample` shards when a `fit` inner axis grows) stay cached and are not rebuilt.
- Add an end-to-end test that grows a `min_pmix` value, re-runs `tar_make()`, and asserts: affected `fit` shards rebuilt and overwritten, prior rows byte-identical, the added row present, untouched `sample` shards cached.

This change adds **no new exported API and no new code paths** — the mechanism is already in place via `task-tables`/`task-shards`. It contributes a spec requirement and a regression test; the precise expected cached-vs-rebuilt set is finalised against the invalidation model pinned by `hive-partitioning` (see Impact and design).

## Capabilities

### New Capabilities
<!-- None: this change adds no new capability; it pins an existing one. -->

### Modified Capabilities
- `task-shards`: adds the explicit inner-axis-growth contract — adding an inner-axis value atomically rewrites the affected shards (whole-file overwrite), the prior rows re-emit byte-identical, and untouched steps stay cached (`TARGETS-DESIGN.md` §8.2).

## Impact

- **New code**: none. The rewrite-on-inner-growth behaviour already follows from static branching + per-layout roots in `R/task-shards.R` / `R/targets-runner.R` (`task-tables`/`task-shards`, landed). This change only adds the contract and its test.
- **Tests**: a new end-to-end regression in `tests/testthat/` that runs a tiny scenario, adds a `min_pmix`, re-runs `tar_make()`, and asserts the affected `fit` shards are overwritten with byte-identical prior rows while the `sample` shards stay cached. Compares re-emitted bytes against the captured prior Parquet.
- **Dependencies (landed)**: builds on `task-tables` (the static-branching pipeline that mints one target per shard and rewrites a shard's Parquet on a task-set change) — landed — and on `task-shards` (the shard-grouping + per-layout root it asserts against).
- **Dependencies (decision)**: **downstream of `hive-partitioning`**. The precise expected cached-vs-rebuilt set follows the invalidation model `hive-partitioning` pins (the §8 cache-by-existence vs. content-hash fork over the m:n child↔parent fan-in). The byte-stable-rewrite contract holds either way, but the test's assertion on *which* shards stay cached is finalised once that decision lands; this change states that dependency explicitly rather than guessing the model.
- **Byte-stability dependency**: the "prior rows come out byte-identical" guarantee rests on deterministic Parquet writes — a fixed/deterministic column order under a **pinned** `duckplyr`/DuckDB. This change calls that dependency out; it does not introduce the pin (that rides with the pipeline's existing write helpers).
- **Downstream**: pins the byte-stability §8.4 (forced re-run after a code fix) and §9 (reproducibility) rely on, and is a prerequisite of `mixed-code-lockin` (§8.3), which pins shards against a code change on top of this rewrite contract. TARGETS-DESIGN.md §12 DAG: `task-tables → shard-atomic-rewrite`, `shard-atomic-rewrite → mixed-code-lockin`.
