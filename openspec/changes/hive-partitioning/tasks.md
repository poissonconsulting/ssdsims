## 1. Per-child upstream edges (Option 3)

- [x] 1.1 Add a sourcing-time helper that, given a child step's shard table and the parent step, returns for each child shard the vector of parent `*_step` target names its tasks read — derived from `unique(path_key(tasks, scenario$partition_by[[parent]]))` (the same projection `read_parent_shards()` uses) rendered through the parent's `tar_map(names = scenario_partition_axes(scenario, parent)$path)` naming
- [x] 1.2 In `ssd_scenario_targets()`, replace the `sample_done` / `fit_done` `tar_combine()` barriers (as the *invalidation* wiring) with per-child edges: each child `*_step` target's command references its specific parent `*_step` target name(s) as symbols (so `targets` records the edges) before calling `ssd_run_<step>_step(...)`
- [x] 1.3 Keep the partition-path read contract unchanged (the runners still open parent Parquets via `read_parent_shards()`); assert in code/comment that the named parent set equals the set `read_parent_shards()` opens (one source of truth)
- [x] 1.4 Order `summary` after the `hc` shards without a per-shard value dependency (a lightweight ordering reference / a terminal combine used for ordering only), preserving its directory-read fan-in

## 2. Invalidation model and cue policy

- [x] 2.1 Confirm/keep `format = "file"` on the shard step targets as the content-hash invalidation surface; document the cache-by-existence reading in the `ssd_scenario_targets()` roxygen
- [x] 2.2 Document and (where the factory exposes it) wire the `tar_cue(depend = FALSE)` pin semantics with the §8.3 carve-outs (missing output, changed grouping, or prior error still rebuilds); note that `tar_invalidate()` / `unlink()` force a refresh overriding the pin (§8.4)

## 3. Settle the data-step-vs-fold decision

- [x] 3.1 Record in the factory roxygen that the `fit`-inline `head(sample, nrow)` fold is retained (no materialised `data` step), keyed by `nrow` in `fit_id`, and that the per-child edge + `n_max` task column handle the widened-draw dual hazard

## 4. Tests

- [x] 4.1 A second `tar_make()` with no input change leaves every shard cached (cache-by-existence hit); deleting one shard's Parquet rebuilds only that shard
- [x] 4.2 Rewriting/invalidating one parent shard re-runs only the child shards that read it (and `summary`), leaving sibling child shards cached — across `sample → fit` and `fit → hc` (m:n fan-in)
- [x] 4.3 The named parent-shard edge set for each child equals the set `read_parent_shards()` opens (graph/read consistency)
- [x] 4.4 `tar_cue(depend = FALSE)` pin holds against a per-task primitive edit, with the carve-outs (missing Parquet / changed grouping / prior error still rebuild); `tar_invalidate()` forces refresh of the chosen shards only
- [x] 4.5 Widening `max(nrow)` rewrites the affected `sample` shard and invalidates exactly the `fit` shards that read it; no `fit` shard reads a stale shorter draw

## 5. Docs, format, and validation

- [x] 5.1 Run `devtools::document()` and `air format .`
- [x] 5.2 Run `devtools::check()` and resolve any new NOTE/WARNING introduced by this change
- [x] 5.3 Run `openspec validate hive-partitioning --strict` and ensure it passes with no errors
