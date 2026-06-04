## 1. Dependency and scaffolding

- [ ] 1.1 Add `duckplyr` to `Imports` in `DESCRIPTION` (brings `duckdb`); run `devtools::document()` / check it installs
- [ ] 1.2 Create `R/shard-runner.R` with the exported runner skeleton `ssd_run_scenario_shards(scenario, dir = tempfile("ssdsims-shards-"))`; validate `scenario` is `ssdsims_scenario` and create `dir`

## 2. Shard grouping and Hive write

- [ ] 2.1 Add an internal helper that, given a step's task table and `scenario_partition_axes(scenario, step)$path`, groups rows into shards (one group per path-axis value cell), reusing `path_key()` for the path string
- [ ] 2.2 Add an internal Hive-path renderer producing `<dir>/<step>/<axis=value>/.../part.parquet` from a shard's path-axis values (reuse `path_key()` so it matches the in-memory `<step>_id` algebra)
- [ ] 2.3 Add a per-shard Parquet writer that writes the shard's inner-axis columns plus the per-task result column; pick the result representation (native nested vs. raw/blob) that round-trips `fitdists`/sample data faithfully (design Open Question)

## 3. Parent-shard read and m:n resolution

- [ ] 3.1 Add an internal helper that, for a child shard, computes the **distinct set of parent shard paths** = each task's `<parent>_id` identity projected onto `scenario_partition_axes(scenario, parent)$path`
- [ ] 3.2 Read those parent shards via `duckplyr::read_parquet_duckdb()` over the parent Hive glob and filter to the parent identities the child tasks reference (predicate pushdown on path + inner columns); deserialise the parent result objects
- [ ] 3.3 Match parent rows to child tasks by the `<parent>_id` identity join (same linkage the in-memory runner does in memory); assert no child task is left without its parent and none is mis-attributed

## 4. Three-step sharded runner

- [ ] 4.1 `sample` step: group `ssd_scenario_sample_tasks()` rows into shards, run each task via `sample_data_task_primer(..., seed, primer)` (primer from `task_primers(tbl, "sample")`), write sample shards
- [ ] 4.2 `fit` step: for each fit shard, read its parent `sample` shards (§3), truncate `head(sample, nrow)` inline (RNG-free), run `fit_data_task_primer(...)`, write fit shards
- [ ] 4.3 `hc` step: for each hc shard, read its parent `fit` shards (§3), run `hc_data_task_primer(...)`, write hc shards
- [ ] 4.4 Open one `local_dqrng_backend()` scope for the run; confirm each task installs its `(seed, primer)` exactly once (per-task RNG unchanged from the in-memory runner)
- [ ] 4.5 Return a manifest of written shard paths per step (and/or the read-back tables) so callers and tests can locate the shards

## 5. Tests

- [ ] 5.1 Oracle test: per-task results equal between `ssd_run_scenario_baseline()` and `ssd_run_scenario_shards()` (join on each step's `<step>_id`)
- [ ] 5.2 Re-layout test (realises `partition-by`'s deferred acceptance test): same scenario under two `partition_by` settings for a step ⇒ shard file paths differ, per-task results byte-identical
- [ ] 5.3 m:n test (fan-in via inner `replace`): `replace = c(FALSE, TRUE)` with `replace` inner at `fit` ⇒ a `fit` shard reads two `sample` shards; results match the oracle
- [ ] 5.4 m:n test (fan-in via coarse child): default split ⇒ an `hc` shard reads several `fit` shards across `nrow`/`rescale`; results match the oracle
- [ ] 5.5 Shard-count test: number of Parquet files per step equals `Π |path axis value|`, not the task count; bundled-task shard contains all its tasks' inner rows
- [ ] 5.6 Reproducibility/order-independence test: two runs with a fixed `scenario$seed` produce identical shards; processing shards in a permuted order does not change results
- [ ] 5.7 Layout test: written files are at the expected Hive paths and read back via duckplyr predicate pushdown returns the right subset without opening unrelated shards

## 6. Docs and checks

- [ ] 6.1 Roxygen for `ssd_run_scenario_shards()`: storage layout, duckplyr read-back, m:n parent resolution, reproducibility; cross-reference `partition-by` (`scenario_partition_axes()`) and note it de-risks `hive-partitioning`/`task-tables`
- [ ] 6.2 Run `devtools::document()`, `air format .`, `devtools::check()`; update `NAMESPACE`/`man/`
- [ ] 6.3 Add `NEWS.md` entry
