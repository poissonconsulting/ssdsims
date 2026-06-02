## Context

`task-list-loop-baseline` (#80) landed a four-step model in `R/task-lists.R`: `sample` (the single RNG draw of `n_max = max(nrow)` rows, keyed `(dataset, sim, replace)`), `data` (RNG-free `head(sample, nrow)`), `fit`, `hc`, linked by path-style `<step>_id`/`<parent>_id` keys. Review raised whether the `data` step earns its place: `head()` is negligible, and the §5 sub-truncation property only needs the *draw* (the `sample` step) to exclude `nrow` — which it does. The decision is to fold the truncation into `fit`. This is a deliberate change to merged code; the user reviews before merging.

## Goals / Non-Goals

**Goals:**

- Remove the `data` task step; `fit` truncates its parent sample inline (`head(sample, nrow)`) before fitting.
- Three steps: `sample` → `fit` → `hc`, with `nrow` a `fit` cross-join axis inherited by `hc`.
- Preserve the §5 sub-truncation property and all RNG behaviour (the `sample` draw is unchanged).
- Keep the `ssdsims_tasks`/`ssdsims_task_set` classes, the `<step>_id`/`<parent>_id` linkage, and the baseline runner — just over three steps.

**Non-Goals:**

- Changing the `sample` or `hc` derivations (other than `fit`'s parent now being `sample` and `hc`'s identity inheriting `nrow` through `fit`).
- Adding per-task RNG seeding, shards, `partition_by`, Parquet, or `targets` (later roadmap steps).
- Re-opening whether `sample` should be a separate step (it must stay separate — that is what keeps `nrow` out of the draw).

## Decisions

### Decision: fold the truncation into `fit`, keep `sample` separate

The sub-truncation guarantee depends only on a single draw keyed `(dataset, sim, replace)`; *where* the `head()` happens is free. So `sample` stays its own step (the draw), and `fit` does `head(sample, nrow)` itself. Concretely in `R/task-lists.R`:

- `task_axes()`: drop the `data` case; `fit = c("dataset", "sim", "replace", "nrow", "rescale", "computable", "at_boundary_ok", "min_pmix", "range_shape1", "range_shape2")`; `hc = c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")`.
- `task_parent()`: `fit`'s parent is `"sample"`; `hc`'s parent stays `"fit"`; `sample` is the root.
- `fit_task_table()`: cross `sample_task_grid()` (carrying `nrow` via `expand_grid` over `scenario$nrow`) with `fit_grid_tbl()`. Remove `ssd_scenario_data_tasks()`, `data_task_grid()`.
- Runner: the `fit` step resolves its parent draw by `sample_id` and fits `utils::head(sample_out[[sample_id]], nrow)`; the standalone `data` loop is gone.
- `ssd_scenario_tasks()` / `ssd_run_scenario_baseline()` return `sample`/`fit`/`hc`.

### Decision: materialising `data` buys nothing for tiny SSD datasets

The only thing a materialised `data` step could buy is *I/O amortisation*: when `n_max ≫ nrow` and the fit grid is wide, a small `data` shard is read by the whole fit fan-out instead of re-reading a larger `sample`. **SSD datasets make that moot.** A `Conc` dataset is a handful to low-hundreds of rows (`nrow` ∈ [5, 1000]), so the `sample` draw is already tiny — there is no large object to avoid re-reading, and `head()` over it is free. So materialisation buys nothing while it costs: a `data` shard duplicates a prefix of its `sample` shard on disk, and the extra task table / shard layer is pure overhead. (Even setting size aside, #80 writes no Parquet — in-process baseline — so there is nothing to amortise yet.) If some future use ever grew datasets enough to matter, the remedy would be a *materialisation* choice at the `hive-partitioning`/`task-tables` layer (cache the truncation as a shard), **not** a separate logical step — the task model stays three steps.

### Decision: replay follows the `sample` parent, unchanged in spirit

A failed `fit`/`hc` task reconstructs its data by seeding the `sample` primer (via the row's `sample_id`), drawing `n_max`, and `head(nrow)` — exactly as before, except the `head()` is part of `fit` rather than a separate `data` task. No `data`-level replay artifact is lost because there never was per-task RNG there.

## Risks / Trade-offs

- **Breaking change to a just-merged export** (`ssd_scenario_data_tasks()` removed; `ssd_scenario_tasks()` set shrinks; `fit` carries `sample_id` not `data_id`). Mitigation: the package is pre-release with no downstream dependants (per `TARGETS-DESIGN.md` §12, breaking steps are fine); snapshots and tests are updated in the same change.
- **Lost I/O amortisation** for wide fit fan-outs with `n_max ≫ nrow` — but SSD datasets are tiny, so there is no large sample to re-read and nothing to amortise (see the decision above). If datasets ever grew, re-introduce as a *materialisation* (shard cache) at the partitioning layer; the three-step model is unaffected.
- **Snapshot churn** in `_snaps/task-lists.md` (task counts and previews change; `data` snapshots removed). Mitigation: regenerate and review the diff.

## Migration Plan

In-place edit of `R/task-lists.R` + tests/snapshots + `man/`/`NAMESPACE`. No data migration. The change is self-contained and revertible. It unblocks the simpler three-step `partition-by` contract; `TARGETS-DESIGN.md` §12 gains a one-line note that `task-list-loop-baseline` was expanded (data folded into fit), with no new DAG node, and §5's step table is annotated accordingly.

## Open Questions

- Should `ssd_scenario_tasks()` keep a `data` *accessor* that lazily computes `head(sample, nrow)` for inspection convenience, or is that needless surface? Leaning needless — callers can `head()` a `sample` row themselves; revisit only if a consumer wants the truncation without fitting.
