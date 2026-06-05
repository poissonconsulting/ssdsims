## 1. Prerequisites

- [x] 1.1 Confirm `hive-partitioning` has pinned the invalidation model (`TARGETS-DESIGN.md` §8 cache-by-existence vs. content-hash); the expected-cached assertions below are finalised against that model
- [x] 1.2 Confirm the landed `task-tables` factory (`ssd_scenario_targets()` in `R/targets-runner.R`) is the baseline this refines, and re-read the three runners to pin the per-step consumed-field set

## 2. Scenario-slice helper

- [x] 2.1 Add the internal `scenario_step_slice(scenario, step)` (in `R/targets-runner.R` next to `shard_path()`/`read_parent_shards()`): return the minimal `ssdsims_scenario`-classed sub-object the named step consumes — `sample` → datasets + `partition_by["sample"]`; `fit` → `fit$dists` (+ fit fields the primer reads) + `min_pmix_fns` + `partition_by[c("sample","fit")]`; `hc` → `hc$proportion` + `hc$samples` + `partition_by[c("fit","hc")]`
- [x] 2.2 Preserve the `ssdsims_scenario` class on the slice so the runners' `chk_s3_class()` and `scenario_dataset()`/`scenario_min_pmix()` calls work unchanged; make the helper a pure function of the scenario (no environment capture) so it is deterministic and hashable
- [x] 2.3 Keep `seed`/`primer` out of the slice (they ride in each shard's `tasks` list-column); keep `min_pmix` functions name-hashing (carried for `fit` but not part of task identity)

## 3. Refactor the factory to splice the slice

- [x] 3.1 In `ssd_scenario_targets()`'s `step_map()`, bind each step's slice (`scenario_step_slice(scenario, step)`) to a per-step value and **splice it** into the command via `rlang::expr()`'s `!!`, replacing the bare `scenario` symbol — `ssd_run_sample_step(tasks, !!sample_slice, !!sample_dir)` and the `fit`/`hc` analogues — leaving the per-child `.parents` edge block, `tasks`, and path-axis `names` untouched
- [x] 3.2 Update the `ssd_scenario_targets()` roxygen: replace "scenario is referenced as a global, so editing it invalidates the dependent shards" with the per-step minimal-slice contract (editing a step-irrelevant field leaves the other steps' shards cached)

## 4. Tests

- [x] 4.1 `scenario_step_slice()`: each step's slice carries exactly its runner's consumed fields and omits the others; the slice is `ssdsims_scenario`-classed and byte-identical across two computations (deterministic/hashable)
- [x] 4.2 End-to-end: `tar_make()` a tiny scenario, change an `hc`-only knob (`hc$samples`), `tar_make()` again, and assert only `hc` (and `summary`) rebuild while `sample`/`fit` shards are skipped (via `tar_progress()`/`tar_outdated()`) — finalised against the `hive-partitioning` model
- [x] 4.3 End-to-end: change a `fit`-only knob (`fit$dists`) and assert `sample` shards stay cached while `fit`/`hc`/`summary` rebuild
- [x] 4.4 Regression: the per-task `sample`/`fit`/`hc` results remain byte-identical to `ssd_run_scenario_baseline()` for the same scenario (the slice drops no consumed field)

## 5. Docs and checks

- [x] 5.1 Run `devtools::document()` (refresh `man/`/`NAMESPACE` if the slice helper's roxygen changes anything), `air format .`, and `devtools::check()`
- [x] 5.2 Run `openspec validate step-scenario-slice --strict` and ensure it passes
