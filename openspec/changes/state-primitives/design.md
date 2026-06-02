## Context

The three-step baseline runner (`ssd_run_scenario_baseline()` in `R/task-lists.R`, from `task-list-loop-baseline` + `…-fold`) uses unseeded inline `dplyr::slice_sample()` / `fit_data_task()` / `hc_data_task()` — it is explicitly *not* reproducible. `TARGETS-DESIGN.md` §2 fixes per-task RNG as `dqrng::dqset.seed(seed = scenario$seed, stream = task_primer(params))`. `local-dqrng-state` (#78) landed `local_dqrng_state(seed, state)`; `task-primer` lands `task_primer()`. `GLOSSARY.md` defines the per-task value as a **primer** and flags the `_state`/`state =` naming of the existing primitives as "a historical misnomer, to be fixed in a future iteration: `primer =` argument of the `_primer` functions." This step adds the dqrng per-task seeding as reusable seed-and-run functions under that corrected naming and wires them into the new runner.

## Goals / Non-Goals

**Goals:**

- A named, self-contained **seed-and-run** wrapper per step that installs `(seed, primer)` once via `local_dqrng_state()`, then runs the state-less op — reusable by the baseline runner, a future `targets` shard body, and `replay-helper` (§7).
- Adopt the corrected GLOSSARY naming for the new functions: `_primer` suffix, `primer` argument (not `_state`/`state`).
- Keep the state-less ops free of any RNG argument; add `sample_data_task()` for symmetry.
- Make `ssd_run_scenario_baseline()` reproducible and order-independent for a fixed `scenario$seed`, with no external seed.
- Preserve the §5 sub-truncation property under seeding.

**Non-Goals:**

- Migrating the public `ssd_sim_data.data.frame` / `ssd_fit_dists_sims` / `ssd_hc_sims` API or the `_seed` shims (`migrate-public-api`).
- Renaming the legacy L'Ecuyer `slice_sample_state`/`fit_dists_state`/`hc_state` or the landed `local_dqrng_state()`/`with_dqrng_state()` argument (`cleanup-lecuyer` / a separate rename).
- Shards, Parquet, `targets`, or changing the task tables / `ci = FALSE` collapse (done in `task-list-loop-baseline`).
- Defining `task_primer()` (`task-primer`) or `local_dqrng_state()` (`local-dqrng-state`, landed).

## Decisions

### Decision: a named seed-and-run wrapper, because targets needs it (not an inline closure)

A `targets` shard body runs in its own worker process and must install the RNG itself for each task — it cannot inherit an ambient `local_dqrng_state()` set by an enclosing loop. So the seed-and-run step has to be a **named, self-contained function**, reused by three callers: the baseline runner (loop over rows), the future shard target (loop over the shard's rows), and `replay-helper` (reproduce one task as a one-liner). An inline closure in the runner would not be reusable across processes.

### Decision: wrap the state-less op; do not add optional RNG args

"Seed-and-run" is **seed + run**, and the seeding is mandatory — there is nothing optional about it. So the wrapper has required `(seed, primer)` and the state-less op stays a separate function with **no** RNG argument (per the §2 rule "no `state =` argument on the inner ops"). Optional RNG args on the op would merge the two and leave a non-reproducible call path as a footgun. Split:

- **ops** (state-less): `sample_data_task(data, n_max, replace)`, `fit_data_task(data, …)`, `hc_data_task(fits, …)`.
- **wrappers** (`*_data_task_primer`): `local_dqrng_state(seed, primer)` once, then the op.

*Alternative considered:* `fit_data_task(data, …, seed = NULL, primer = NULL)` — rejected; conflates responsibilities, breaks the inner-op rule, and keeps an unseeded path alive.

### Decision: `_primer` suffix and `primer` argument — the corrected GLOSSARY naming

The per-task value is a **primer** (GLOSSARY): the thing that, with `seed`, picks the task's independent stream. The GLOSSARY explicitly calls the legacy `_state`/`state =` naming a misnomer and names the fix — `_primer` functions with a `primer` argument. Since these wrappers are new code, they land the corrected naming directly: `sample_data_task_primer()` / `fit_data_task_primer()` / `hc_data_task_primer()` with a `primer` argument. This also pairs cleanly with #80's `*_data_task` ops and sidesteps the name collision with the legacy L'Ecuyer `slice_sample_state`/`fit_dists_state`/`hc_state` (which keep their names until `cleanup-lecuyer`). The lower-level `local_dqrng_state(seed, state)` keeps its `state` argument for now (a #78 leftover where `_state` is partly legitimate — it scopes the real generator state); the wrapper passes `local_dqrng_state(seed, state = primer)`. *Alternatives considered:* keep `_state`/`state` for consistency with `local_dqrng_state` — rejected; it perpetuates the documented misnomer in fresh code. A verb suffix like `_primed` — viable if `_primer` reads ambiguously against `task_primer()` (which *produces* a primer), but `_primer` matches the GLOSSARY's stated convention; flagged as an easy swap if preferred.

### Decision: wrapper takes `(seed, primer)`, caller computes the primer

The wrapper signature is `(op inputs…, seed, primer)` where `primer` is the already-computed per-task primer. The **caller** (runner / shard body) does `primer <- task_primer(<canonical row identity>)` and passes it, keeping the wrappers schema-agnostic — they know nothing about task-table columns. The per-step identity is the `task_axes(step)` columns (sample: `dataset, sim, replace`; fit: + `nrow` + fit grid; hc: + hc grid), assembled name-keyed (the `task-primer` caller contract). Replay is then a one-liner: `fit_data_task_primer(data, …, seed, task_primer(params))`.

### Decision: one backend scope per run; one primer install per task

`ssd_run_scenario_baseline()` opens a single `local_dqrng_backend()` scope for the whole run (reentrant; reset on exit/error). Inside, each task calls its wrapper, which installs its own `(seed, primer)` via `local_dqrng_state()`. Because each task's pair fully determines its RNG, results are reproducible and order-independent — no external `set.seed()`. One outer backend scope (not one per task) avoids repeated register/restore churn; per-task isolation comes from `local_dqrng_state()`. The runner's inline `dplyr::slice_sample` / `fit_data_task` / `hc_data_task` calls are replaced by the wrappers; `min_pmix` name resolution and the `ci = FALSE` branch are preserved.

## Risks / Trade-offs

- **`task-primer` not yet applied** → cannot implement until `task_primer()` exists. Mitigation: explicit dependency; apply `task-primer` first.
- **Two naming layers for the primer** (`primer` on the new wrappers, `state` on the landed `local_dqrng_state()`) → mild inconsistency. Mitigation: the wrapper passes `state = primer` explicitly and documents the leftover; the `local_dqrng_state()` argument rename is a separate, later change.
- **Reproducibility regressions in `test-task-lists.R`** → the runner gains seeding, so any value-level expectation must be re-pinned to the new dqrng streams. Mitigation: assert reproducibility/structure (equality across two runs; prefix property) over hard-coded values; snapshot any value-level expectations under the new contract.
- **Two primitive families** (L'Ecuyer `*_state` in `internal.R`, dqrng `*_data_task_primer` in the new runner) → potential confusion. Mitigation: distinct names; the GLOSSARY update points the corrected naming at the new functions; `cleanup-lecuyer` removes the legacy set.

## Migration Plan

Additive: the `*_data_task_primer()` wrappers + `sample_data_task()` op + runner wiring + a `GLOSSARY.md` naming note; the legacy primitives and public API are untouched. No data migration. The runner becomes reproducible without an external seed (a behaviour gain, not a breaking API change). Revertible by restoring the runner's inline ops and removing the wrappers.

## Open Questions

- Should `task-tables` (a later step) materialise a `(seed, primer)` column on each task row that the runner/shard body reads, instead of computing `task_primer()` inline? The wrapper contract (`(seed, primer)`) is unaffected either way; lean: compute inline now, switch to a materialised column when `task-tables` lands.
- `_primer` vs `_primed` suffix, given `task_primer()` *produces* a primer while these *consume/install* one — keep `_primer` (GLOSSARY convention) unless the maintainer prefers the verb form.
