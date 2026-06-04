## Context

`primer-primitives` (archived) established the per-task RNG contract for the targets path: a state-less op (`sample_data_task()` / `fit_data_task()` / `hc_data_task()`) wrapped by a seed-and-run `*_data_task_primer()` that calls `local_dqrng_state(seed, task_primer(identity))` exactly once under a reentrant `local_dqrng_backend()`. The declarative baseline runner already runs on it.

The legacy public surface does not. `ssd_sim_data.data.frame()`, `ssd_fit_dists_sims()`, and `ssd_hc_sims()` still seed per task through the **L'Ecuyer-CMRG sub-stream lattice** in `R/internal.R`: `get_lecuyer_cmrg_stream_state(s)(seed, stream, start_sim)` produces a length-7 `.Random.seed` state, installed by `with_lecuyer_cmrg_state()` inside the `*_state` ops (`slice_sample_state`, `fit_dists_state`, `hc_state`) and their `*_seed` wrappers (`fit_dists_seed`, `hc_seed`). `ssd_run_scenario()` chains these three, and already opens a `local_dqrng_backend()` it does not yet use for seeding.

This change is the §12 `migrate-public-api` step: a **cosmetic, non-gating** carry-over of those three functions onto the dqrng + primer contract, with the L'Ecuyer helpers kept as a one-release shim. The benchmark of correctness is the loop-free `scripts/example-expanded.R`, which currently proves the L'Ecuyer public path byte-equals a hand expansion built from the `*_state` primitives; after migration it must prove the same equality on the dqrng primitives.

## Goals / Non-Goals

**Goals:**
- Route the per-task seeding of the three named public functions through `local_dqrng_state(seed, task_primer(identity))` under `local_dqrng_backend()`, reusing the `*_data_task_primer()` wrappers where signatures line up.
- Keep the public **signatures and argument semantics** unchanged (including function-valued `min_pmix`, `stream`, `start_sim`, `nsim`).
- Preserve the §5 sub-truncation property: one shared `n_max` draw per `(stream, sim, replace)`, `head()`-truncated per `nrow`.
- Re-establish `scripts/example-expanded.R` byte-equivalence on the new primitives, backed by a test.
- Leave the L'Ecuyer `*_state`/`*_seed` helpers in place (deprecated) for the generator methods, the `*-grids` scripts, and existing tests.

**Non-Goals:**
- Migrating the generator methods `ssd_sim_data.function`/`.character`/`.fitdists`/`.tmbfit` (they keep `do_call_seed`/L'Ecuyer; the declarative path handles generators via `scenario-input-types`/`scenario-accessors`).
- Removing the L'Ecuyer helpers — that is `cleanup-lecuyer`.
- Preserving the *numeric values* of the old L'Ecuyer output (impossible across generators; intended).
- Adding `min_pmix`-by-name resolution to the public surface (out of scope; that is `scenario-accessors`).
- Changing the declarative/targets primitives, which are frozen by `primer-primitives`.

## Decisions

### D1 — Reuse the seed-and-run pattern, not necessarily each `*_data_task_primer()` symbol
The `sample` and `hc` steps map cleanly onto `sample_data_task_primer()` and `hc_data_task_primer()` (their grids match). The `fit` step does **not**: `fit_data_task()` resolves `min_pmix` **by name** via `resolve_min_pmix()` (the name-keyed declarative contract, now `scenario-accessors`), whereas the public `ssd_fit_dists_sims()` accepts a *list of functions* — including user-defined anonymous functions that have no resolvable name. Forcing the public function through the name-based primitive would break its documented `min_pmix = list(...)` contract.

**Decision:** preserve the public function-valued `min_pmix`. The migrated `ssd_fit_dists_sims()` adopts the *contract* (open backend → `local_dqrng_state(seed, primer)` once per task → run the state-less fit against the ambient RNG) using a thin function-valued seed-and-run path (the existing fit body minus the L'Ecuyer wrapper), rather than `fit_data_task_primer()`. `sample`/`hc` reuse the shipped wrappers directly.

- *Alternative A (rejected):* change the public `min_pmix` to a name/string routed through `resolve_min_pmix()`. This is a public behavior change beyond "cosmetic" and breaks anonymous-function `min_pmix`; it belongs with `scenario-accessors`, not here.
- *Alternative B (rejected):* extend `fit_data_task()` to accept either a name or a function. Touches the frozen `primer-primitives` primitives and blurs the declarative contract.

### D2 — Per-task primer identity reuses `task_primer()` over the task's own columns
Each function hashes the task identity it already has. To stay consistent with the §2 / `parallel-safe-seeding` keying:
- **sample**: `task_primer(list(stream, sim, replace))` — **not** `nrow` (the shared-draw / `head` property, §5).
- **fit**: parent `(stream, sim)` extended with the fit-grid row (`nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix` **name**, `range_shape1`, `range_shape2`).
- **hc**: parent `(stream, sim)` extended with the hc-grid row (`ci`, `nboot`, `est_method`, `ci_method`, `parametric`).

The legacy schema uses `stream`/`sim` where the declarative tables use `dataset`/`sim`; the legacy functions have no `dataset` name, so `stream` stands in as the per-run lattice coordinate. `min_pmix` is keyed by name (or a stable label when a function carries no name) so JIT/recompile does not move the primer. Byte-equivalence in `example-expanded.R` only requires *internal* consistency between the public path and its expansion, both built from these identities — so the exact identity composition is a free choice as long as both sides agree.

### D3 — Each public function opens its own `local_dqrng_backend()`
The three functions are exported and callable standalone, so each must activate the backend itself. `local_dqrng_backend()` is **reentrant** (a nested call is a no-op, detected via `RNGkind()[1] == "user-supplied"`; see `AGENTS.md` / `dqrng-init`), so when `ssd_run_scenario()` (or `run_scenario()`, which already opens one) calls them, only the outermost scope activates and resets — the RNG stream is identical with or without nesting. The backend (and `local_dqrng_state()`) restore the caller's prior RNG kind and state on exit, preserving the "Seed independence from caller RNG state" guarantee.

### D4 — `seed = NULL` draws a one-time scalar seed from the ambient RNG
`local_dqrng_state()` requires a whole-number `seed`, but the public default is `seed = NULL` (L'Ecuyer meaning: "use the current global RNG state"). To keep both reproducible-when-set and random-when-unset without leaving global state moved, when `seed = NULL` the function SHALL draw a single scalar integer seed from the ambient RNG once at entry (inside the to-be-restored scope) and use it for every task in the call. A supplied `seed` is used verbatim. This mirrors the L'Ecuyer NULL-seed intent on the dqrng path.

### D5 — Keep the L'Ecuyer helpers as a deprecated shim; rewrite the reference scripts
`slice_sample_state()`, `fit_dists_state()`, `fit_dists_seed()`, `hc_state()`, `hc_seed()`, `do_call_seed()`, and `get_lecuyer_cmrg_stream_state(s)()` stay defined (Roxygen-noted as deprecated, removed by `cleanup-lecuyer`) — they still back the generator `ssd_sim_data` methods and the `*-grids` scripts/tests. `scripts/example-expanded.R` is rewritten to build its three per-step lists from `sample_data_task_primer()`/`fit_data_task_primer()`/`hc_data_task_primer()` and to compare against the migrated `ssd_run_scenario()` (`data`/`hc` by `identical()`, `fits` by `ssdtools::estimates()`, as today). The `example-expanded-grids*.R` variants — which explored independent-per-call sub-streams — are reconciled with the dqrng model, where each task already has its own primer (the "independent" design *is* the dqrng model), or annotated as historical.

## Risks / Trade-offs

- **Numeric output changes for a fixed `seed` (L'Ecuyer → dqrng).** → Intended and acceptable pre-1.0; called out as BREAKING (numeric) in the proposal and in `NEWS.md`. Snapshot tests keyed to the old values are updated; reproducibility *for a fixed seed* and order-independence are the new, tested guarantees.
- **`fit` not reusing `fit_data_task_primer()` risks drift from the declarative path.** → Mitigated by sharing the *contract* (one `local_dqrng_state()` per task) and by the byte-equivalence test; documented in D1 so a future `scenario-accessors`-era cleanup can converge the two.
- **`stream` semantics quietly change** (L'Ecuyer top-level stream → primer component). → Signature unchanged; documented in Roxygen and the modified `parallel-safe-seeding` spec; distinct-stream independence is preserved and tested.
- **`seed = NULL` behavior change** (D4). → Documented; the global-RNG-restored property is preserved and tested; reproducibility for explicit seeds is unaffected.
- **Stale references to the L'Ecuyer helpers.** → They remain defined (shim), so the generator methods and `*-grids` scripts keep working until `cleanup-lecuyer`.

## Migration Plan

1. Add the function-valued `fit` seed-and-run path (D1) and confirm `sample`/`hc` reuse the shipped wrappers.
2. Migrate `ssd_sim_data.data.frame()` (backend + `n_max`/`head` + `(stream,sim,replace)` primer), then `ssd_fit_dists_sims()`, then `ssd_hc_sims()`.
3. Implement D4 (`seed = NULL`) consistently across the three.
4. Rewrite `scripts/example-expanded.R`; reconcile the `*-grids` variants.
5. Update tests (reproducibility, order-independence, global-RNG-restored, byte-equivalence vs. the expansion), refresh snapshots, update Roxygen/`man/`, `GLOSSARY.md`, and `NEWS.md`.
6. Run `air format`, `R CMD check` / `devtools::test()`.

Rollback is a straight revert: the L'Ecuyer helpers are untouched, so reverting the three functions restores the prior behavior.

## Open Questions

- **D1 (`fit` min_pmix):** confirm the public `min_pmix` stays function-valued here and name-based resolution is deferred to `scenario-accessors`. (Assumed yes — preserves the public contract; the "cosmetic" framing covers only the seeding mechanism.)
- **D4 (`seed = NULL`):** confirm the "draw one scalar seed from the ambient RNG" semantics over alternatives (fixed default seed; abort when NULL). (Assumed the draw-once approach to match L'Ecuyer intent.)
- **`*-grids` scripts:** keep and reconcile, or retire as historical now that dqrng makes per-task-own-stream the default? (Assumed keep + annotate; cheap and documents the equivalence.)
