## 1. Spec delta

- [x] 1.1 Confirm the `task-shards` delta adds the new requirement *Path-axis growth mints new shards and caches existing ones* under `## ADDED Requirements`, with one `#### Scenario:` for dataset growth and one for `nsim` growth (both WHEN/THEN)
- [x] 1.2 Confirm the requirement states the dependency on `hive-partitioning`'s invalidation model (the §8 cache-by-existence vs. content-hash fork) and is written to hold under either model

## 2. End-to-end growth test

- [x] 2.1 Add `tests/testthat/test-path-axis-growth.R`: build a tiny scenario via `ssd_define_scenario()` with a small `dists`/grid, source `ssd_scenario_targets(scenario)` into a throwaway `targets` store under `withr::local_tempdir()`, and `tar_make()` to completion
- [x] 2.2 Capture the original shards' deterministic target names (derived from `ssd_scenario_sample_shards()`/`_fit_shards()`/`_hc_shards()`, not hard-coded) and their Parquet bytes
- [x] 2.3 Append a dataset to the scenario (leaving `partition_by` unchanged), re-source, and `tar_make()` again into the same store/root; assert via `tar_progress()`/`tar_meta()` that only the new dataset's `sample`/`fit`/`hc` shards are `built`, every original shard is `skipped` with byte-identical Parquet, and `summary` is `built`
- [x] 2.4 Repeat 2.3 for `nsim` growth: grow `nsim`, re-source, `tar_make()`; assert only the added `sim` cells' shards build, prior `sim` shards are skipped (byte-identical), and `summary` re-runs
- [x] 2.5 Assert the `summary` row count reflects the added shards (so a spuriously byte-stable summary that did not re-read the enlarged set is caught)

## 3. Validation and checks

- [x] 3.1 Once `hive-partitioning` has pinned the invalidation model, lock the test's precise `tar_progress()` skip/build enumeration to that model
- [x] 3.2 Run `devtools::document()`, `air format .`, `devtools::check()`, and `openspec validate path-axis-growth --strict`
