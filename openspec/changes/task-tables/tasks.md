## 1. Dependencies

- [x] 1.1 Add `duckplyr` to `DESCRIPTION` `Imports` (the runners call it) and `targets`/`tarchetypes` to `Suggests` (used only by the shipped `_targets.R` template and the gated integration tests, so `Imports` would trip an "unused import" NOTE); add the `ssd_read_parquet()` / `ssd_write_parquet()` internals wrapping `duckplyr` (`read_parquet_duckdb()` / `compute_parquet()`)
- [x] 1.2 Confirm prerequisites are in place: `scenario_partition_axes()` and the three-step `sample`/`fit`/`hc` defaults (`partition-by`), and the `scenario_dataset()` / `scenario_min_pmix()` accessors (`scenario-accessors`) (`manifest` is **not** a prerequisite)

## 2. Shard grouping wrappers

- [x] 2.1 Add `ssd_scenario_sample_shards()` / `_fit_shards()` / `_hc_shards()` in `R/task-shards.R`: derive the step's task table, group by `scenario_partition_axes(scenario, step)$path`, one shard row per path cell carrying the path-axis columns and a `tasks` list-column
- [x] 2.2 Decorate each task row in `tasks` with `seed = scenario$seed` and `primer = task_primers(tbl, step)` (RNG-free; reuse the existing helper over `task_axes(step)`)
- [x] 2.3 Carry each task row's upstream partition-path columns in `tasks` so the runner can open its parent shard
- [x] 2.4 Reuse `path_key()` to render each shard's Hive partition path; do not duplicate the split logic from `partition-by`

## 3. Per-shard step runners

- [x] 3.1 Add `ssd_run_sample_step(tasks, scenario, out_dir)`: under one `local_dqrng_backend()`, read the dataset via `scenario_dataset(scenario, dataset)`, loop tasks calling `sample_data_task_primer()`, write one Parquet at the shard's partition path
- [x] 3.2 Add `ssd_run_fit_step(tasks, scenario, sample_dir, out_dir)`: read the upstream `sample` Parquet by partition path, `head(sample, nrow)` inline (RNG-free), call `fit_data_task_primer()` (resolving `min_pmix` via `scenario_min_pmix()`), write one Parquet
- [x] 3.3a Reuse the baseline runner's per-step loop body (the per-task `*_data_task_primer()` calls) in the step runners — only the shard subsetting and Parquet I/O are new
- [x] 3.3 Add `ssd_run_hc_step(tasks, scenario, fit_dir, out_dir)`: read the upstream `fit` Parquet(s) by partition path, call `hc_data_task_primer()`, write one Parquet

## 4. Summary fan-in

- [x] 4.1 Add `ssd_summarize(dir_sample, dir_fit, dir_hc, path)`: read the result layers with `duckplyr` and write `results/summary.parquet`, without depending on shard target values or recomputing upstream

## 5. Targets pipeline template

- [x] 5.1 Add `inst/targets-templates/local/_targets.R`: build the scenario as a construction-time object, compute the three `*_shards()` tables at sourcing time, and `tar_map()` one named target per shard per step (`format = "file"`, `error = "null"`), plus a `summary` target
- [x] 5.2 Document static branching and the construction-time scenario (§6); note whole-shard `error = "null"` gating is in scope, while *partial* survival (shorter shards, survivor-union, `warn = 2`) and the Hive layout are deferred to `shard-failure-survival`/`hive-partitioning`

## 6. Docs and reference

- [x] 6.1 Roxygen for the shard wrappers, step runners, and `ssd_summarize()`; cross-reference §5/§6 and the prerequisite capabilities
- [x] 6.2 Add a "Targets pipeline" reference group to `_pkgdown.yml`

## 7. Tests and checks

- [x] 7.1 `tests/testthat/test-task-shards.R`: one shard row per path cell; union of `tasks` equals the full task table; coarser `partition_by` bundles more tasks per shard
- [x] 7.2 Shard task rows carry `(seed, primer)` equal to `task_primer()` over the identity; derivation draws no random numbers (`.Random.seed` unchanged)
- [x] 7.3 Step-runner tests: a shard writes one Parquet; `fit`/`hc` read upstream by partition path; dataset/`min_pmix` come from the scenario accessors
- [x] 7.3b `error = "null"` test: a deterministically-failing whole shard leaves the other shards built, its error is readable via `tar_meta()`, and `ssd_summarize()` unions the survivors
- [x] 7.4 Integration test (skip when `targets` unavailable / on CRAN): copy the template into `tempdir()`, source it for a tiny scenario, `tar_make()`, assert every shard target completes and one Parquet per shard exists
- [x] 7.5 Byte-identity test (the oracle): the pipeline's read-back per-task `sample`/`fit`/`hc` results equal the single-core `ssd_run_scenario_baseline()` for the same scenario, after sorting both sides by the task-identity key (`<step>_id`) to normalise the unordered Parquet read
- [x] 7.6 `ssd_summarize()` reads landed shards and writes a combined summary without recomputation
- [x] 7.7 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`

## 8. Per-layout results root

- [x] 8.1 Add the exported `scenario_results_dir(scenario, root)` helper (`<root>/layout=<hash(partition_by)>`); the shipped `_targets.R` template writes every step's shards and the summary under it, so a changed `partition_by`/`bundle` never mixes shard granularities in one root (Option D). Test that the root differs by `partition_by` and is stable under non-layout knobs

## 9. Target factory

- [x] 9.1 Add the exported `ssd_scenario_targets(scenario, root = scenario_results_dir(scenario))` target factory returning the full target list (per-shard `tar_map`s with `names` from `scenario_partition_axes()$path`, `tar_combine` barriers, `summary`), so a `_targets.R` is just `source("scenario.R"); ssd_scenario_targets(scenario)`; both shipped templates use it; covered by the integration test
