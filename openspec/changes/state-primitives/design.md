## Context

`R/internal.R` holds the legacy per-task primitives `slice_sample_state()` / `fit_dists_state()` / `hc_state()`, each wrapping its operation in `with_lecuyer_cmrg_state(state, …)` (length-7 L'Ecuyer state). They serve the old `ssd_run_scenario()` / `ssd_sim_data` path. The new three-step baseline runner (`ssd_run_scenario_baseline()` in `R/task-lists.R`, from `task-list-loop-baseline` + `…-fold`) instead uses unseeded inline `dplyr::slice_sample()` / `fit_data_task()` / `hc_data_task()` — it is explicitly *not* reproducible. `TARGETS-DESIGN.md` §2 fixes per-task RNG as `dqrng::dqset.seed(seed = scenario$seed, stream = task_primer(params))`. `local-dqrng-state` (#78) landed `local_dqrng_state(seed, state)`; `task-primer` lands `task_primer()`. This step installs the dqrng + primer contract in the new runner.

## Goals / Non-Goals

**Goals:**

- dqrng-seeded `slice_sample_state()` / `fit_dists_state()` / `hc_state()`: install `(seed, primer)` once via `local_dqrng_state()`, then the state-less op.
- Wire them into `ssd_run_scenario_baseline()` so each task seeds exactly once from `task_primer(<row identity>)`, making the runner reproducible and order-independent for a fixed `scenario$seed`.
- Preserve the §5 sub-truncation property under seeding (seeded `n_max` draw + inline `head`).

**Non-Goals:**

- Migrating the public `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` API or the `_seed` shims (`migrate-public-api`).
- Removing the legacy L'Ecuyer `_state`/`_seed` primitives (`cleanup-lecuyer`).
- Shards, Parquet, `targets`, or changing the task tables / `ci = FALSE` collapse (done in `task-list-loop-baseline`).
- Defining `task_primer()` itself (`task-primer`) or `local_dqrng_state()` (`local-dqrng-state`, landed).

## Decisions

### Decision: new dqrng primitives live beside the legacy ones, not in place

The legacy `R/internal.R` primitives still back `ssd_run_scenario()` until `cleanup-lecuyer`. Re-pointing them at dqrng now would change the legacy path's RNG mid-stream and break its snapshots before `migrate-public-api` is ready. So the dqrng versions go in a new `R/state-primitives.R` and are used by the new runner only; the L'Ecuyer ones are untouched. *Alternative considered:* overwrite the internal.R bodies to call `local_dqrng_state()` — rejected; it couples this step to migrating the legacy public API (a separate roadmap item) and risks snapshot churn there. The one-release coexistence is the intended overlap (CLAUDE.md RNG discipline).

### Decision: `slice_sample_state` draws `n_max`; truncation stays in `fit`

After `…-fold`, the draw and the truncation are separate: `slice_sample_state(data, n_max, seed, state, replace)` is purely the seeded `n_max` draw, and the `fit` step does `head(sample, nrow)` inline (RNG-free) before fitting. So `slice_sample_state` needs no `n`/`nrow` argument — it absorbs the former `nrow-sub-truncation` work as just "seed, then draw `n_max`". The prefix property then follows from `head()` over one shared draw and is tested for both `replace` values. *Alternative considered:* a `slice_sample_state(data, n_max, n, …)` that draws and truncates in one call (the §5 sketch) — unnecessary here because the fold already separates the draw (one `sample` task) from the truncation (`fit`), and a single shared draw object is cleaner than re-deriving the prefix per `nrow`.

### Decision: the runner installs one primer per task, scoped by `local_dqrng_backend()`

`ssd_run_scenario_baseline()` opens one `local_dqrng_backend()` scope for the whole run (reentrant; reset on exit/error), then for each task computes `primer <- task_primer(<canonical row identity>)` and calls the matching `_state` primitive with `seed = scenario$seed` and `state = primer`. Because each task's `(seed, primer)` fully determines its RNG, results are reproducible and order-independent — no external `set.seed()` needed. The canonical identity per step is the task's `task_axes()` columns (sample: `dataset, sim, replace`; fit: + `nrow` + fit grid; hc: + hc grid), assembled name-keyed (the `task-primer` caller contract). *Why one backend scope for the whole run, not per task?* `local_dqrng_backend()` is reentrant and per-task `local_dqrng_state()` already isolates each task; a single outer scope avoids repeated register/restore churn.

### Decision: replace the runner's inline ops with the primitives

The runner's current `dplyr::slice_sample(...)`, `fit_data_task(...)`, and `hc_data_task(...)` internals are replaced by `slice_sample_state()` / `fit_dists_state()` / `hc_state()` calls (the latter two subsuming `fit_data_task`/`hc_data_task`, which had the same bodies minus seeding). `min_pmix` name resolution (`resolve_min_pmix()`) and the `ci = FALSE` branch in the hc body are preserved.

## Risks / Trade-offs

- **`task-primer` not yet applied** → this change cannot be implemented until `task_primer()` exists. Mitigation: explicit dependency; apply `task-primer` first (the proposal and §12 order reflect this).
- **Reproducibility regressions in `test-task-lists.R`** → the runner gains seeding, so any test asserting specific draw values must be re-pinned to the new dqrng streams (snapshots). Mitigation: tests assert reproducibility/structure (equality across two runs, prefix property) rather than hard-coded values where possible; snapshot any value-level expectations under the new contract.
- **Two coexisting primitive sets** (L'Ecuyer in `internal.R`, dqrng in `state-primitives.R`) → potential confusion. Mitigation: docs/comment cross-reference both, mark dqrng as the path forward; `cleanup-lecuyer` removes the legacy set.

## Migration Plan

Additive: a new `R/state-primitives.R` plus runner wiring; the legacy primitives and public API are untouched. No data migration. The new runner becomes reproducible without an external seed (a behaviour gain, not a breaking API change). Revertible by restoring the runner's inline ops and removing the new file.

## Open Questions

- Should the per-task primer identity be assembled by the runner inline, or should `task-tables` (a later step) attach a `primer` column to each task row that the runner just reads? Leaning: the runner assembles it from the row here (no task-table schema change yet); `task-tables` can later materialise a `(seed, primer)` column and the runner switches to reading it, with no change to the primitives' contract.
