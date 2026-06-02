## Why

`task-list-loop-baseline` (#80) introduced a four-step task model (`sample` → `data` → `fit` → `hc`) where `data` is a standalone RNG-free `head(sample, nrow)` truncation. On reflection the separate `data` step is not justified. **SSD datasets are tiny** — a handful to low-hundreds of `Conc` rows (`nrow` ∈ [5, 1000]) — so the `sample` draw is already small: the one thing a materialised `data` shard could buy (I/O amortisation: read a small truncation instead of re-reading a big sample across the fit fan-out) buys **nothing** here, because there is no big sample to re-read. Materialising `data` only *costs* — it duplicates data on disk (a `data` shard is literally a prefix of its `sample` shard) and adds a whole task table and shard layer, for a step whose only consumer is `fit`. And `head()` compute is negligible, while the §5 sub-truncation property only requires the **`sample`** draw to be keyed `(dataset, sim, replace)` without `nrow` — which is preserved regardless of where the truncation happens. Folding the truncation into `fit` removes the table, the shard layer, and the redundancy, and simplifies the downstream `partition-by` knob from four steps to three.

## What Changes

- **BREAKING (internal task shape):** Remove the `data` task step. The model becomes three steps: `sample` → `fit` → `hc`.
- `fit` truncates inline: it reads its parent `sample` draw (via `sample_id`) and applies `head(sample, nrow)` (RNG-free) before calling `ssd_fit_dists()`. `fit`'s parent is now `sample`.
- `nrow` becomes a `fit` cross-join axis (`fit = sample identity + nrow + fit grid`) and is inherited by `hc`. The `sample` draw remains keyed `(dataset, sim, replace)` with no `nrow`, so the shared-draw / sub-truncation property is unchanged.
- `R/task-lists.R`: drop `ssd_scenario_data_tasks()`, `data_task_grid()`, and the `data_id` key; update `task_axes()` (`fit = c(sample, "nrow", <fit grid>)`, no `data`); update `task_parent()` (`fit`'s parent is `sample`); make the `fit` runner do `head(sample_out[[sample_id]], nrow)`; update `ssd_scenario_tasks()`/`ssd_run_scenario_baseline()` to the `sample`/`fit`/`hc` set; the `ssdsims_task_set` exposes three tables.
- Update tests and snapshots (`tests/testthat/test-task-lists.R`, `_snaps/task-lists.md`).
- Mention in `TARGETS-DESIGN.md` §12 that the already-applied `task-list-loop-baseline` change was expanded (data folded into fit) — without adding a new DAG node.

## Capabilities

### New Capabilities
<!-- None: this modifies the existing task-lists capability. -->

### Modified Capabilities
- `task-lists`: drops the `data` task-table requirement; `fit` now crosses the `sample` identity with `nrow` and the fit grid and truncates inline; the id/foreign-key chain becomes `sample ← fit ← hc`; the compound expansion and baseline runner cover three steps, not four.

## Impact

- **Code**: `R/task-lists.R` (remove the `data` derivation; rewire `task_axes()`/`task_parent()`/the runner/`ssd_scenario_tasks()`); `tests/testthat/test-task-lists.R` and its snapshot.
- **APIs**: `ssd_scenario_data_tasks()` is **removed** (a breaking change to #80's just-added export); `ssd_scenario_tasks()` returns a three-element set; `fit` tasks carry `nrow` + `sample_id` (not `data_id`). `man/`/`NAMESPACE` updated.
- **Dependencies**: Modifies code merged via #80; user reviews before merging.
- **Downstream**: Simplifies the in-flight `partition-by` change to three steps (`sample`/`fit`/`hc`), with `nrow` a `fit`/`hc` path axis. `task-primer` is unaffected in substance (the `data` step already took no primer; `nrow` already lives on the `fit`/`hc` primers via inheritance). `TARGETS-DESIGN.md` §5/§12 get a note, not a graph change.
