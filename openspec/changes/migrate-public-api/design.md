## Context

`primer-primitives` (archived) established the per-task RNG contract for the targets path: a state-less op (`sample_data_task()` / `fit_data_task()` / `hc_data_task()`) wrapped by a seed-and-run `*_data_task_primer()` that calls `local_dqrng_state(seed, task_primer(identity))` exactly once under a reentrant `local_dqrng_backend()`. The declarative baseline runner already runs on it.

The legacy public surface does not. `ssd_sim_data.data.frame()`, `ssd_fit_dists_sims()`, and `ssd_hc_sims()` still seed per task through the **L'Ecuyer-CMRG sub-stream lattice** in `R/internal.R`: `get_lecuyer_cmrg_stream_state(s)(seed, stream, start_sim)` produces a length-7 `.Random.seed` state, installed by `with_lecuyer_cmrg_state()` inside the `*_state` ops (`slice_sample_state`, `fit_dists_state`, `hc_state`) and their `*_seed` wrappers (`fit_dists_seed`, `hc_seed`). `ssd_run_scenario()` chains these three, and already opens a `local_dqrng_backend()` it does not yet use for seeding.

This change is the §12 `migrate-public-api` step: a **cosmetic** carry-over of those three functions onto the dqrng + primer contract, with the L'Ecuyer helpers kept as a one-release shim. Per the roadmap it has **no dependants** and depends on `primer-primitives` (the contract) and on `scenario-input-types` (so its byte-equivalence re-run can exercise the full public input surface; `inputs → migrate`). The benchmark of correctness is the loop-free `scripts/example-expanded.R`, which currently proves the L'Ecuyer public path byte-equals a hand expansion built from the `*_state` primitives; after migration it must prove the same equality on the dqrng primitives.

## Goals / Non-Goals

**Goals:**
- Route the per-task seeding of the three named public functions through `local_dqrng_state(seed, task_primer(identity))` under `local_dqrng_backend()`, reusing the `*_data_task_primer()` wrappers where signatures line up.
- Keep the public **signatures** unchanged except `min_pmix`, which adopts the name-based pattern (`stream`, `start_sim`, `nsim`, the fit/hc grids, etc. unchanged).
- Preserve the §5 sub-truncation property: one shared `n_max` draw per `(stream, sim, replace)`, `head()`-truncated per `nrow`.
- Re-establish `scripts/example-expanded.R` byte-equivalence on the new primitives, backed by a test.
- Leave the L'Ecuyer `*_state`/`*_seed` helpers in place (deprecated) for the generator methods, the `*-grids` scripts, and existing tests.

**Non-Goals:**
- Migrating the generator methods `ssd_sim_data.function`/`.character`/`.fitdists`/`.tmbfit` (they keep `do_call_seed`/L'Ecuyer; the declarative path handles generators via `scenario-input-types`/`scenario-accessors`).
- Removing the L'Ecuyer helpers — that is `cleanup-lecuyer`.
- Preserving the *numeric values* of the old L'Ecuyer output (impossible across generators; intended).
- Building a `min_pmix` *registry* on the scenario (that is `scenario-accessors`); here `ssd_fit_dists_sims()` resolves names with the existing `resolve_min_pmix()`.
- Changing the declarative/targets primitives, which are frozen by `primer-primitives`.

## Decisions

### D1 — Adopt the name-based `min_pmix` pattern so all three steps reuse `*_data_task_primer()`
The `sample` and `hc` steps map cleanly onto `sample_data_task_primer()` and `hc_data_task_primer()`. The `fit` step's only mismatch is `min_pmix`: `fit_data_task()` resolves it **by name** via `resolve_min_pmix()` (the name-keyed declarative contract, `scenario-accessors`), whereas the legacy public `ssd_fit_dists_sims()` took a *list of functions*.

**Decision:** adapt the public surface to the name pattern. `ssd_fit_dists_sims(min_pmix = ...)` takes a character vector of function **names** (default `"ssd_min_pmix"`), resolved with the existing `resolve_min_pmix()`, so the migrated function reuses `fit_data_task_primer()` **directly** — exactly like the declarative path. Consistent patterns and code reuse outweigh preserving the function-valued surface; the API change is minor and acceptable pre-1.0. Name-based identity is also the reproducibility-correct form already mandated by `parallel-safe-seeding` (a recompile/JIT must not move a task's primer), and custom user functions stay supported: `resolve_min_pmix()` falls back to the global environment, so a named `my_min_pmix` in scope resolves via `"my_min_pmix"`.

- *Alternative (rejected):* keep function-valued `min_pmix` and run `fit` through a parallel function-accepting seed-and-run helper. Rejected: it duplicates `fit_data_task_primer()`, diverges from the declarative path, and an anonymous function has no stable name to hash (defeating the §2 primer contract).

### D2 — Per-task primer identity reuses `task_primer()` over the task's own columns
Each function hashes the task identity it already has. To stay consistent with the §2 / `parallel-safe-seeding` keying:
- **sample**: `task_primer(list(stream, sim, replace))` — **not** `nrow` (the shared-draw / `head` property, §5).
- **fit**: parent `(stream, sim)` extended with the fit-grid row (`nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix` **name**, `range_shape1`, `range_shape2`).
- **hc**: parent `(stream, sim)` extended with the hc-grid row (`ci`, `nboot`, `est_method`, `ci_method`, `parametric`).

The legacy schema uses `stream`/`sim` where the declarative tables use `dataset`/`sim`; the legacy functions have no `dataset` name, so `stream` stands in as the per-run lattice coordinate. `min_pmix` is keyed by its **name** (D1), so JIT/recompile does not move the primer. Byte-equivalence in `example-expanded.R` only requires *internal* consistency between the public path and its expansion, both built from these identities — so the exact identity composition is a free choice as long as both sides agree.

### D3 — Each public function opens its own `local_dqrng_backend()`
The three functions are exported and callable standalone, so each must activate the backend itself. `local_dqrng_backend()` is **reentrant** (a nested call is a no-op, detected via `RNGkind()[1] == "user-supplied"`; see `AGENTS.md` / `dqrng-init`), so when `ssd_run_scenario()` (or `run_scenario()`, which already opens one) calls them, only the outermost scope activates and resets — the RNG stream is identical with or without nesting. The backend (and `local_dqrng_state()`) restore the caller's prior RNG kind and state on exit, preserving the "Seed independence from caller RNG state" guarantee.

### D4 — `seed = NULL` draws a one-time scalar seed from the ambient RNG
`local_dqrng_state()` requires a whole-number `seed`, but the public default is `seed = NULL` (L'Ecuyer meaning: "use the current global RNG state"). To keep both reproducible-when-set and random-when-unset without leaving global state moved, when `seed = NULL` the function SHALL draw a single scalar integer seed from the ambient RNG once at entry (inside the to-be-restored scope) and use it for every task in the call. A supplied `seed` is used verbatim. This mirrors the L'Ecuyer NULL-seed intent on the dqrng path.

### D5 — Keep the L'Ecuyer helpers as a deprecated shim; rewrite the reference scripts
`slice_sample_state()`, `fit_dists_state()`, `fit_dists_seed()`, `hc_state()`, `hc_seed()`, `do_call_seed()`, and `get_lecuyer_cmrg_stream_state(s)()` stay defined (Roxygen-noted as deprecated, removed by `cleanup-lecuyer`) — they still back the generator `ssd_sim_data` methods and the `*-grids` scripts/tests. `scripts/example-expanded.R` is rewritten to build its three per-step lists from `sample_data_task_primer()`/`fit_data_task_primer()`/`hc_data_task_primer()` and to compare against the migrated `ssd_run_scenario()` (`data`/`hc` by `identical()`, `fits` by `ssdtools::estimates()`, as today). The `example-expanded-grids*.R` variants — which explored independent-per-call sub-streams — are reconciled with the dqrng model, where each task already has its own primer (the "independent" design *is* the dqrng model), or annotated as historical.

## Risks / Trade-offs

- **Numeric output changes for a fixed `seed` (L'Ecuyer → dqrng).** → Intended and acceptable pre-1.0; called out as BREAKING (numeric) in the proposal and in `NEWS.md`. Snapshot tests keyed to the old values are updated; reproducibility *for a fixed seed* and order-independence are the new, tested guarantees.
- **`min_pmix` API change** (list-of-functions → character vector of names). → Minor and pre-1.0; lets `fit` reuse `fit_data_task_primer()` directly (no parallel path); documented in `NEWS.md` and the `fit-distributions` spec delta. Custom functions remain usable by name via `resolve_min_pmix()`'s global-environment fallback; an informative error fires when a name cannot be resolved.
- **`stream` semantics quietly change** (L'Ecuyer top-level stream → primer component). → Signature unchanged; documented in Roxygen and the modified `parallel-safe-seeding` spec; distinct-stream independence is preserved and tested.
- **`seed = NULL` behavior change** (D4). → Documented; the global-RNG-restored property is preserved and tested; reproducibility for explicit seeds is unaffected.
- **Stale references to the L'Ecuyer helpers.** → They remain defined (shim), so the generator methods and `*-grids` scripts keep working until `cleanup-lecuyer`.

## Migration Plan

1. Switch `ssd_fit_dists_sims(min_pmix=)` to a character vector of names resolved by `resolve_min_pmix()` (D1), so all three steps reuse the shipped `*_data_task_primer()` wrappers directly.
2. Migrate `ssd_sim_data.data.frame()` (backend + `n_max`/`head` + `(stream,sim,replace)` primer), then `ssd_fit_dists_sims()`, then `ssd_hc_sims()`.
3. Implement D4 (`seed = NULL`) consistently across the three.
4. Rewrite `scripts/example-expanded.R`; reconcile the `*-grids` variants.
5. Update tests (reproducibility, order-independence, global-RNG-restored, byte-equivalence vs. the expansion), refresh snapshots, update Roxygen/`man/`, `GLOSSARY.md`, and `NEWS.md`.
6. Run `air format`, `R CMD check` / `devtools::test()`.

Rollback is a straight revert: the L'Ecuyer helpers are untouched, so reverting the three functions restores the prior behavior.

## Open Questions

- **D4 (`seed = NULL`):** confirm the "draw one scalar seed from the ambient RNG" semantics over alternatives (fixed default seed; abort when NULL). (Assumed the draw-once approach to match L'Ecuyer intent.)
- **`*-grids` scripts:** keep and reconcile, or retire as historical now that dqrng makes per-task-own-stream the default? (Assumed keep + annotate; cheap and documents the equivalence.)
