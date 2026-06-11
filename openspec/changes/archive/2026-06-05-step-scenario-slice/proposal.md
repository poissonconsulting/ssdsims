## Why

`ssd_scenario_targets()` (in `R/targets-runner.R`) leaves `scenario` as a **bare symbol** in every step command — `ssd_run_sample_step(tasks, scenario, …)`, and likewise for `fit` and `hc`. Because `targets` tracks that symbol as a global, the **whole scenario object is a dependency of every shard target across all three steps**. Editing *any* scenario field therefore invalidates and rebuilds *all* shards, even a scenario option that feeds only one step — e.g. `scenario$hc$samples` (an `hc`-only output option) or `scenario$fit$dists` (a `fit`-only scenario option) — when only that step's shards depend on it. `TARGETS-DESIGN.md` §12 records this as a pre-existing caveat that this change closes; it is the per-step counterpart of the minimal-rebuild contract `path-axis-growth` asserts along the path axis.

The step runners already consume only a small, fixed slice of the scenario each (see below), so the bare-symbol coupling is conservative, not necessary. Projecting each command onto the **minimal slice** its step actually reads lets a step-irrelevant edit leave the other steps' shards cached.

## What Changes

- Add a deterministic, hashable **scenario-slice helper** `scenario_step_slice(scenario, step, datasets)` that returns the minimal sub-object the named step's per-shard runner consumes — the resolved per-step inputs and the fields that reach that step's per-task body and its `shard_path()`/`read_parent_shards()` primer:
  - `sample` → the `datasets` it draws from (read via `scenario_dataset()`) + `partition_by$sample`.
  - `fit` → `fit$dists` + `min_pmix_fns` (resolved via `scenario_min_pmix()`) + `partition_by` for `sample` and `fit` (parent + own path).
  - `hc` → `hc$proportion` + `hc$samples` + `partition_by` for `fit` and `hc`.
  - (`seed`/`primer` ride in each shard's `tasks` list-column and are **not** part of the slice.)
- Refactor `ssd_scenario_targets()` so each step command depends on its slice instead of the bare `scenario`. The `fit`/`hc` slices carry no datasets and are step-global, spliced once via `!!`. The `sample` slice carries datasets, so it is built **per shard** — carrying only the dataset(s) that shard reads (`unique(tasks$dataset)`) — and carried as a per-shard `.slice` mapped value alongside `tasks`/`.parents`. Editing a field outside a step's slice no longer invalidates that step's shards; and because each `sample` shard depends only on its own dataset, **appending a dataset mints a new shard and leaves every existing shard cached** (the path-axis-growth payoff, which the bare global — and a whole-`data` slice — both defeated).
- The per-task results SHALL be unchanged: the slice carries exactly the fields the runner reads today.

## Capabilities

### Modified Capabilities
- `task-shards`: a new requirement — each step target depends only on its **minimal scenario slice**, so editing a step-irrelevant field leaves the other steps' shards cached (the per-step half of the minimal-rebuild contract). Added as `## ADDED Requirements` to avoid colliding with other in-flight `task-shards` deltas.
- `scenario-accessors`: a small new requirement recording the internal `scenario_step_slice()` as a member of the accessor family (it isolates *part* of a scenario for a step's runner, deterministically and hashing-stably) — optional sibling of `scenario_dataset()`/`scenario_min_pmix()`. Also `## ADDED Requirements`.

### New Capabilities
<!-- None: the slice helper is an internal refinement of the task-shards factory; no new capability spec. -->

## Impact

- **New code**: `scenario_step_slice(scenario, step)` (internal, in `R/targets-runner.R` next to `shard_path()`/`read_parent_shards()`, or `R/accessors.R`); a refactor of `ssd_scenario_targets()`'s `step_map()` + the three `rlang::expr()` step commands. The runners' signatures are unchanged — they still receive a scenario-shaped object, now the slice.
- **APIs**: no new exports (the slice helper is internal); `ssd_scenario_targets()`'s contract is unchanged from the caller's side.
- **Invalidation model**: the expected-cached set is **finalised against the invalidation model pinned by `hive-partitioning`** (the §8 cache-by-existence vs. content-hash fork). This change owns the *per-step input* dependency edge; `hive-partitioning` owns how a shard's *value* propagates. See design.
- **Dependencies (direction)**: **depends on** `hive-partitioning` (pins the invalidation model the assertion finalises against) and on the landed `task-tables` (the `ssd_scenario_targets()` factory this refines). **Pairs with** `path-axis-growth` (the path-axis counterpart of the same minimal-rebuild contract). TARGETS-DESIGN.md §12 DAG: `task-tables → step-scenario-slice`, `hive-partitioning → step-scenario-slice`.
- **When to land it**: after `hive-partitioning` finalises the invalidation model; it is a pure cache-granularity refinement of an existing factory, so it never blocks `cluster-pipeline` or the toy pipeline.
