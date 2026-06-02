## Context

The three-step baseline runner (`ssd_run_scenario_baseline()` in `R/task-lists.R`, from `task-list-loop-baseline` + `…-fold`) uses unseeded inline `dplyr::slice_sample()` / `fit_data_task()` / `hc_data_task()` — it is explicitly *not* reproducible. `TARGETS-DESIGN.md` §2 fixes per-task RNG as `dqrng::dqset.seed(seed = scenario$seed, stream = task_primer(params))`. `local-dqrng-state` (#78) landed `local_dqrng_state(seed, state)`; `task-primer` lands `task_primer()`. The legacy `R/internal.R` already holds L'Ecuyer-CMRG `slice_sample_state()` / `fit_dists_state()` / `hc_state()` for the old `ssd_run_scenario()` path — so those names are taken. This step adds the dqrng per-task seeding as a reusable seed-and-run function and wires it into the new runner.

## Goals / Non-Goals

**Goals:**

- A named, self-contained **seed-and-run** wrapper per step that installs `(seed, primer)` once via `local_dqrng_state()`, then runs the state-less op — reusable by the baseline runner, a future `targets` shard body, and `replay-helper` (§7).
- Keep the state-less ops free of any RNG argument; add `sample_data_task()` for symmetry.
- Make `ssd_run_scenario_baseline()` reproducible and order-independent for a fixed `scenario$seed`, with no external seed.
- Preserve the §5 sub-truncation property under seeding.

**Non-Goals:**

- Migrating the public `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` API or the `_seed` shims (`migrate-public-api`).
- Touching or renaming the legacy L'Ecuyer `slice_sample_state`/`fit_dists_state`/`hc_state` (`cleanup-lecuyer`).
- Shards, Parquet, `targets`, or changing the task tables / `ci = FALSE` collapse (done in `task-list-loop-baseline`).
- Defining `task_primer()` (`task-primer`) or `local_dqrng_state()` (`local-dqrng-state`, landed).

## Decisions

### Decision: a named seed-and-run wrapper, because targets needs it (not an inline closure)

A `targets` shard body runs in its own worker process and must install the RNG itself for each task — it cannot inherit an ambient `local_dqrng_state()` set by an enclosing loop. So the seed-and-run step has to be a **named, self-contained function**, reused by three callers: the baseline runner (loop over rows), the future shard target (loop over the shard's rows), and `replay-helper` (reproduce one task as a one-liner). An inline closure in the runner would not be reusable across processes. *This overturns an earlier sketch* that seeded inline in the runner and deferred naming.

### Decision: wrap the state-less op; do not add optional RNG args

"Seed-and-run" is **seed + run**, and the seeding is mandatory — there is nothing optional about it. So the wrapper has required `(seed, state)` and the state-less op stays a separate function with **no** `state`/`stream` argument (per the §2 rule "no `state =` argument on the inner ops"). Optional RNG args on the op would merge the two and leave a non-reproducible call path as a footgun. Split:

- **ops** (state-less): `sample_data_task(data, n_max, replace)`, `fit_data_task(data, …)`, `hc_data_task(fits, …)`.
- **wrappers** (`*_data_task_state`): `local_dqrng_state(seed, state)` once, then the op.

*Alternative considered:* `fit_data_task(data, …, seed = NULL, state = NULL)` — rejected; conflates responsibilities, breaks the inner-op rule, and keeps an unseeded path alive.

### Decision: `*_data_task_state` names — pair with #80, leave the legacy family alone

#80 established `fit_data_task()` / `hc_data_task()`. The wrappers take the matching `*_data_task_state()` names (and we add `sample_data_task()` + `sample_data_task_state()` for symmetry). This keeps the `_state` suffix (the GLOSSARY "installs the primer" marker), reads consistently with the merged code, and — crucially — **avoids the collision** with the legacy L'Ecuyer `slice_sample_state`/`fit_dists_state`/`hc_state`, which stay untouched until `cleanup-lecuyer`. *Alternative considered:* reclaim the canonical `_state` names now by renaming the legacy family — rejected for this step; it churns the old path and is `cleanup-lecuyer`'s job.

### Decision: wrapper takes `(seed, primer)`, caller computes the primer

The wrapper signature is `(op inputs…, seed, state)` where `state` is the already-computed primer. The **caller** (runner / shard body) does `primer <- task_primer(<canonical row identity>)` and passes it, keeping the wrappers schema-agnostic — they know nothing about task-table columns. The per-step identity is the `task_axes(step)` columns (sample: `dataset, sim, replace`; fit: + `nrow` + fit grid; hc: + hc grid), assembled name-keyed (the `task-primer` caller contract). Replay is then a one-liner: `fit_data_task_state(data, …, seed, task_primer(params))`.

### Decision: one backend scope per run; one primer install per task

`ssd_run_scenario_baseline()` opens a single `local_dqrng_backend()` scope for the whole run (reentrant; reset on exit/error). Inside, each task calls its wrapper, which installs its own `(seed, primer)` via `local_dqrng_state()`. Because each task's pair fully determines its RNG, results are reproducible and order-independent — no external `set.seed()`. One outer backend scope (not one per task) avoids repeated register/restore churn; per-task isolation comes from `local_dqrng_state()`. The runner's inline `dplyr::slice_sample` / `fit_data_task` / `hc_data_task` calls are replaced by the wrappers; `min_pmix` name resolution and the `ci = FALSE` branch are preserved.

## Risks / Trade-offs

- **`task-primer` not yet applied** → cannot implement until `task_primer()` exists. Mitigation: explicit dependency; apply `task-primer` first.
- **Reproducibility regressions in `test-task-lists.R`** → the runner gains seeding, so any value-level expectation must be re-pinned to the new dqrng streams. Mitigation: assert reproducibility/structure (equality across two runs; prefix property) over hard-coded values; snapshot any value-level expectations under the new contract.
- **Two primitive families** (L'Ecuyer `*_state` in `internal.R`, dqrng `*_data_task_state` in the new runner) → potential confusion. Mitigation: the names are distinct and the dqrng ones pair with #80's ops; docs cross-reference; `cleanup-lecuyer` removes the legacy set.

## Migration Plan

Additive: the `*_data_task_state()` wrappers + `sample_data_task()` op + runner wiring; the legacy primitives and public API are untouched. No data migration. The runner becomes reproducible without an external seed (a behaviour gain, not a breaking API change). Revertible by restoring the runner's inline ops and removing the wrappers.

## Open Questions

- Should `task-tables` (a later step) materialise a `(seed, primer)` column on each task row that the runner/shard body reads, instead of computing `task_primer()` inline? The wrapper contract (`(seed, state)`) is unaffected either way; lean: compute inline now, switch to a materialised column when `task-tables` lands.
