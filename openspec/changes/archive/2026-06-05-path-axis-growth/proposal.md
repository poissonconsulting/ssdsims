## Why

The extensibility goal in `TARGETS-DESIGN.md` rests on a **cheap-extension** payoff (§8.1): appending a dataset — or growing `nsim` — to a scenario that has already been `tar_make()`'d should mint *new* shard targets at the next sourcing and leave **every existing shard cached** (skipped), while only the `summary` fan-in re-runs over the larger shard set. Because a dataset (and a `sim` value) is a **path axis** for all three steps, the new values hash to new partition cells and therefore new `tar_map()` targets; the unchanged shards' commands and `format = "file"` outputs are untouched, so `targets` diffs the target set and builds only the new ones (§6, the static-branching "more named targets" property). This is the payoff the whole extensibility story leans on, and the targets lab confirmed it on its `static-extend` axis (§6) — but the package has **no end-to-end test** asserting it on the real `ssd_scenario → ssd_scenario_*_shards → tar_map → tar_make` path. The mechanism works; the explicit contract does not exist. This change is the **path-axis counterpart** of `shard-atomic-rewrite`'s inner-axis test, and it owns the incremental-rebuild contract that `task-tables` deliberately left out (so that step stays a build-and-compile checkpoint).

## What Changes

- Add a new `task-shards` requirement — *Path-axis growth mints new shards and caches existing ones* — pinning the §8.1 contract: re-sourcing a `tar_make()`'d scenario after appending a dataset (or growing `nsim`) SHALL build only the new path cells' shards across all three steps, SHALL skip every shard that existed before, and SHALL re-run the `summary` fan-in over the enlarged shard set. No new code or exports: this change asserts the behaviour of the shipped factory (`ssd_scenario_targets()`) and runners; the deliverable is the requirement plus its end-to-end test.
- Add `tests/testthat/test-path-axis-growth.R`: run a tiny scenario to completion via `tar_make()`; append a dataset; `tar_make()` again and assert (via `tar_progress()`/`tar_meta()`) only the new dataset's `sample`/`fit`/`hc` shards build, the original dataset's shard targets are skipped, and `summary` re-runs; repeat the assertion for `nsim` growth (new `sim` path cells).

## Capabilities

### New Capabilities
<!-- None: no new capability; this change adds a requirement to an existing one. -->

### Modified Capabilities
- `task-shards`: adds the path-axis-growth incremental-rebuild requirement (new shards built, existing shards cached, `summary` re-runs) — the cheap-extension contract §8.1 names, end-to-end on the shipped factory.

## Impact

- **New code**: none. **Tests**: `tests/testthat/test-path-axis-growth.R` (a multi-`tar_make()` growth test using a throwaway `targets` store under `withr::local_tempdir()`).
- **APIs**: no new exports; exercises the shipped `ssd_scenario_targets()`, `ssd_scenario_*_shards()`, the per-shard step runners, and `ssd_summarize()`.
- **On-disk layout**: none introduced; the test reuses the per-layout `scenario_results_dir()` root (a path-axis growth keeps the same `partition_by`, so the same layout root is reused — that reuse is *what* lets existing shards stay cached).
- **Dependencies (direction)**: **Depends on** `task-tables` (the factory and runners it exercises) and on `task-tables` having **landed** (it has). It is also **downstream of `hive-partitioning`** (which pins the invalidation model the assertion locks against — cache-by-existence over the per-child upstream edges) and of `step-scenario-slice` (whose **per-dataset `sample` slice** is what keeps an appended dataset from rebuilding the existing `sample` shards; `nsim` is in no step's slice, so it never did). **Both have now landed**, so the contract holds on the shipped *default* factory — no `cue` needed — and the test's exact skip/build enumeration is locked to that pinned model. Pairs with `shard-atomic-rewrite` (the inner-axis counterpart). TARGETS-DESIGN.md §12 DAG: `task-tables → path-axis-growth`, finalised against `hive-partitioning` and `step-scenario-slice`.
- **When to land it**: after `hive-partitioning` and `step-scenario-slice` land (both have), so the expected-cached set is final; it gates none of the cluster work and adds no runtime dependency.
