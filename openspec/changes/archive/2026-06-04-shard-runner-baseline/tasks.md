## 1. Dependency and scaffolding

- [x] 1.1 Add `duckplyr` to `Imports` in `DESCRIPTION` (brings `duckdb`); run `devtools::document()` / check it installs
- [x] 1.2 Create `R/shard-runner.R` with the exported runner skeleton `ssd_run_scenario_shards(scenario, dir = tempfile("ssdsims-shards-"))`; validate `scenario` is `ssdsims_scenario` and create `dir`

## 2. Shard grouping and Hive write

- [x] 2.1 Add an internal helper that, given a step's task table and `scenario_partition_axes(scenario, step)$path`, groups rows into shards (one group per path-axis value cell), reusing `path_key()` for the path string
- [x] 2.2 Add an internal Hive-path renderer producing `<dir>/<step>/<axis=value>/.../part.parquet` from a shard's path-axis values (reuse `path_key()` so it matches the in-memory `<step>_id` algebra)
- [x] 2.3 Add a per-shard Parquet writer that writes the shard's inner-axis columns plus the per-task result column; pick the result representation (native nested vs. raw/blob) that round-trips `fitdists`/sample data faithfully (design Open Question)

## 3. Parent-shard read and m:n resolution

- [x] 3.1 Add an internal helper that, for a child shard, computes the **distinct set of parent shard paths** = each task's `<parent>_id` identity projected onto `scenario_partition_axes(scenario, parent)$path`
- [x] 3.2 Read those parent shards via `duckplyr::read_parquet_duckdb()` over the parent Hive glob and filter to the parent identities the child tasks reference (predicate pushdown on path + inner columns); deserialise the parent result objects
- [x] 3.3 Match parent rows to child tasks by the `<parent>_id` identity join (same linkage the in-memory runner does in memory); assert no child task is left without its parent and none is mis-attributed

## 4. Three-step sharded runner

- [x] 4.1 `sample` step: group `ssd_scenario_sample_tasks()` rows into shards, run each task via `sample_data_task_primer(..., seed, primer)` (primer from `task_primers(tbl, "sample")`), write sample shards
- [x] 4.2 `fit` step: for each fit shard, read its parent `sample` shards (§3), truncate `head(sample, nrow)` inline (RNG-free), run `fit_data_task_primer(...)`, write fit shards
- [x] 4.3 `hc` step: for each hc shard, read its parent `fit` shards (§3), run `hc_data_task_primer(...)`, write hc shards
- [x] 4.4 Open one `local_dqrng_backend()` scope for the run; confirm each task installs its `(seed, primer)` exactly once (per-task RNG unchanged from the in-memory runner)
- [x] 4.5 Return a manifest of written shard paths per step (and/or the read-back tables) so callers and tests can locate the shards

## 5. Tests

- [x] 5.1 Oracle test: per-task results equal between `ssd_run_scenario_baseline()` and `ssd_run_scenario_shards()` (join on each step's `<step>_id`)
- [x] 5.2 Re-layout test (realises `partition-by`'s deferred acceptance test): same scenario under two `partition_by` settings for a step ⇒ shard file paths differ, per-task results byte-identical
- [x] 5.3 m:n test (fan-in via inner `replace`): `replace = c(FALSE, TRUE)` with `replace` inner at `fit` ⇒ a `fit` shard reads two `sample` shards; results match the oracle
- [x] 5.4 m:n test (fan-in via coarse child): default split ⇒ an `hc` shard reads several `fit` shards across `nrow`/`rescale`; results match the oracle
- [x] 5.5 Shard-count test: number of Parquet files per step equals `Π |path axis value|`, not the task count; bundled-task shard contains all its tasks' inner rows
- [x] 5.6 Reproducibility/order-independence test: two runs with a fixed `scenario$seed` produce identical shards; processing shards in a permuted order does not change results
- [x] 5.7 Layout test: written files are at the expected Hive paths and read back via duckplyr predicate pushdown returns the right subset without opening unrelated shards

## 6. Docs and checks

- [x] 6.1 Roxygen for `ssd_run_scenario_shards()`: storage layout, duckplyr read-back, m:n parent resolution, reproducibility; cross-reference `partition-by` (`scenario_partition_axes()`) and note it de-risks `hive-partitioning`/`task-tables`
- [x] 6.2 Run `devtools::document()`, `air format .`, `devtools::check()`; update `NAMESPACE`/`man/`
- [x] 6.3 `NEWS.md` entry — generated from the PR title at release time by fledge (`NEWS.md` is fledge-managed and not hand-edited), so no manual entry is added here

## 7. Output-tree ownership

- [x] 7.1 `ssd_run_scenario_shards()` clears each `<dir>/<step>` subtree before writing (Option A), so a re-run with a changed `partition_by`/`bundle` never leaves stale-granularity shards; test that a re-layout into the same `dir` leaves only the current layout's shards
