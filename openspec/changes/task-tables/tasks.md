## 1. Dependencies

- [ ] 1.1 Add `targets` and `tarchetypes` to `DESCRIPTION` `Imports` (`duckplyr` — Parquet I/O — arrives with `registry`)
- [ ] 1.2 Confirm prerequisites are in place: `scenario_partition_axes()` and the three-step `sample`/`fit`/`hc` defaults (`partition-by`) and the `registry` resolver (`manifest` is **not** a prerequisite)

## 2. Shard grouping wrappers

- [ ] 2.1 Add `ssd_scenario_sample_shards()` / `_fit_shards()` / `_hc_shards()` in `R/task-shards.R`: derive the step's task table, group by `scenario_partition_axes(scenario, step)$path`, one shard row per path cell carrying the path-axis columns and a `tasks` list-column
- [ ] 2.2 Decorate each task row in `tasks` with `seed = scenario$seed` and `primer = task_primers(tbl, step)` (RNG-free; reuse the existing helper over `task_axes(step)`)
- [ ] 2.3 Carry each task row's upstream partition-path columns in `tasks` so the runner can open its parent shard
- [ ] 2.4 Reuse `path_key()` to render each shard's Hive partition path; do not duplicate the split logic from `partition-by`

## 3. Per-shard step runners

- [ ] 3.1 Add `ssd_run_sample_step(tasks, scenario, out_dir)`: under one `local_dqrng_backend()`, loop tasks calling `sample_data_task_primer()`, write one Parquet at the shard's partition path
- [ ] 3.2 Add `ssd_run_fit_step(tasks, scenario, sample_dir, out_dir)`: read the upstream `sample` Parquet by partition path, `head(sample, nrow)` inline (RNG-free), call `fit_data_task_primer()` (resolving `min_pmix`/datasets through the `registry`), write one Parquet
- [ ] 3.3 Add `ssd_run_hc_step(tasks, scenario, fit_dir, out_dir)`: read the upstream `fit` Parquet(s) by partition path, call `hc_data_task_primer()`, write one Parquet

## 4. Summary fan-in

- [ ] 4.1 Add `ssd_summarize(dir_sample, dir_fit, dir_hc, path)`: read the result layers with `duckplyr` and write `results/summary.parquet`, without depending on shard target values or recomputing upstream

## 5. Targets pipeline template

- [ ] 5.1 Add `inst/targets-templates/local/_targets.R`: build the scenario as a construction-time object, compute the three `*_shards()` tables at sourcing time, and `tar_map()` one named target per shard per step (`format = "file"`), plus a `summary` target
- [ ] 5.2 Document static branching and the construction-time scenario (§6); note `error = "null"`/survival and Hive layout are deferred to `shard-failure-survival`/`hive-partitioning`

## 6. Docs and reference

- [ ] 6.1 Roxygen for the shard wrappers, step runners, and `ssd_summarize()`; cross-reference §5/§6 and the prerequisite capabilities
- [ ] 6.2 Add a "Targets pipeline" reference group to `_pkgdown.yml`

## 7. Tests and checks

- [ ] 7.1 `tests/testthat/test-task-shards.R`: one shard row per path cell; union of `tasks` equals the full task table; coarser `partition_by` bundles more tasks per shard
- [ ] 7.2 Shard task rows carry `(seed, primer)` equal to `task_primer()` over the identity; derivation draws no random numbers (`.Random.seed` unchanged)
- [ ] 7.3 Step-runner tests: a shard writes one Parquet; `fit`/`hc` read upstream by partition path; sha256 recorded on success
- [ ] 7.4 Integration test (skip when `targets` unavailable / on CRAN): copy the template into `tempdir()`, source it for a tiny scenario, `tar_make()`, assert every shard target completes and one Parquet per shard exists
- [ ] 7.5 Byte-identity test: the pipeline's read-back per-task `sample`/`fit`/`hc` results equal `ssd_run_scenario_baseline()` for the same scenario
- [ ] 7.6 `ssd_summarize()` reads landed shards and writes a combined summary without recomputation
- [ ] 7.7 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE`/`man/`
