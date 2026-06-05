## 1. Spec contract

- [x] 1.1 Confirm the `task-shards` delta (`specs/task-shards/spec.md`) adds the requirement "Inner-axis growth atomically rewrites the affected shards byte-stably" with a `#### Scenario:` pinning: affected `fit` shards rebuilt and overwritten (whole-file, no append), prior rows byte-identical to the prior Parquet, the added task row present, untouched `sample` shards cached
- [x] 1.2 State in the requirement/design the byte-stability dependency on deterministic Parquet writes (fixed column order, pinned `duckplyr`/DuckDB) and the downstream dependency on `hive-partitioning`'s invalidation model for the precise cached-vs-rebuilt set

## 2. Regression test (no new code)

- [x] 2.1 Add an end-to-end test in `tests/testthat/` that builds a tiny scenario, runs `tar_make()` into a per-layout root, and captures each affected `fit` shard's Parquet bytes before growth
- [x] 2.2 Grow the scenario's `fit` grid by one `min_pmix` (a `fit` inner axis), re-source the pipeline, and re-run `tar_make()` into the **same** per-layout root (the layout is unchanged because `min_pmix` is not a `partition_by` axis)
- [x] 2.3 Assert each affected `fit` shard is rebuilt and its Parquet overwritten as a whole (single Parquet per shard, no in-place append)
- [x] 2.4 Assert the rows present before the growth read back byte-identical to the captured prior Parquet (prior tasks' `(seed, primer)` unchanged), the shard differing only by the added `min_pmix` task row(s)
- [x] 2.5 Assert the `sample` shards (a step the growth does not touch) are reported cached by `targets` and are NOT rebuilt; sequence any cached-vs-rebuilt assertion that depends on the invalidation model behind `hive-partitioning`

## 3. Validation and checks

- [x] 3.1 Run `devtools::document()`, `air format .`, and `devtools::check()`; ensure no API/`NAMESPACE`/`man/` changes were introduced (this change adds no code)
- [x] 3.2 Run `openspec validate shard-atomic-rewrite --strict` and confirm it passes
