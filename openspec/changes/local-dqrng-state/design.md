## Context

`dqrng-init` landed the dqrng pcg64 backend and the scenario-scoped `register_methods()` / `restore_methods()` discipline (`TARGETS-DESIGN.md` §2). Per-task RNG is configured by `dqrng::dqset.seed(seed = scenario$seed, stream = primer)`. The §2 execution sketch shows each task body doing exactly one `dqset.seed(seed, stream = task$primer)` before running. To keep that call scoped and restorable — and to satisfy CLAUDE.md's "prefer `local_*`, restore RNG state on exit" discipline — we need a `withr`-style wrapper, the dqrng analogue of the existing `local_lecuyer_cmrg_state()`.

The legacy helper (`R/lecuyer-cmrg-seed.R`) is the model: `local_lecuyer_cmrg_state(state, .local_envir = parent.frame())` captures the current RNG state, installs the new one, and `withr::defer(set_state(old), envir = .local_envir)` restores on frame exit. The dqrng wrapper follows the same shape but takes `(seed, state)` and seeds via `dqset.seed()`.

This step delivers only the primitive. The *value* of `state` (the primer derived from a task hash) is `task-primer`'s job; wiring it into the per-task operations is `state-primitives`. The L'Ecuyer path stays until `cleanup-lecuyer`.

## Goals / Non-Goals

**Goals:**
- `local_dqrng_state(seed, state, .local_envir = parent.frame())` wrapping `dqset.seed(seed, stream = state)` with restore-on-exit.
- Restore the prior RNG state when the calling frame exits, so a call is side-effect-free on the surrounding stream — the same contract as `local_lecuyer_cmrg_state()`.
- `chk` validation of `seed` / `state` / `.local_envir`, including the §2 `NA_integer_` (INT_MIN) primer encoding.
- Optional `with_dqrng_state(seed, state, code)` companion for symmetry.

**Non-Goals:**
- Deriving `state` from a task hash (`task-primer`).
- Refactoring `slice_sample_state()` / `fit_dists_state()` / `hc_state()` onto dqrng (`state-primitives`).
- Removing the L'Ecuyer-CMRG helpers (`cleanup-lecuyer`).
- Activating the dqrng backend — that is `dqrng-init`'s scenario-scoped `register_methods()`; this wrapper assumes the backend is active (or is exercised under it in tests).

## Decisions

- **Decision: `(seed, state)` signature, `state` = primer.** §2 fixes the per-task seeding as `dqset.seed(seed = scenario$seed, stream = primer)`. The wrapper takes both so a per-task body installs its starting point in one call. The argument is named `state` (matching the `_state` suffix convention: the wrapper installs the primer *as the running state*); GLOSSARY records that the value handed to dqrng's `stream` is a *primer*. *Alternative*: a `local_dqrng_seed(seed)` taking only a scalar — rejected; the per-task contract needs the primer/stream too.
- **Decision: capture-and-restore the RNG state via `withr::defer()`.** Mirror `local_lecuyer_cmrg_state()`: snapshot the RNG state on entry, install the new one, defer the restore to `.local_envir` exit. This honours the RNG-discipline rule and makes the primitive composable. *Alternative*: leave the stream advanced (no restore) — rejected; it violates CLAUDE.md's restore-on-exit rule and makes nested use surprising.
- **Decision: snapshot mechanism for dqrng state.** dqrng's generator state is process-internal; the restore captures and reinstates it (e.g. via dqrng's get/set-state facilities, or by re-seeding to a captured `(seed, state)`). The implementation picks the mechanism that round-trips a draw sequence exactly; tested by the "surrounding RNG stream undisturbed" scenario. Because `dqrng-init` also routes base R RNG through dqrng, the snapshot must cover whatever state the active backend exposes. *Alternative*: restore only base R's `.Random.seed` — rejected; with `register_methods()` active the relevant state is dqrng's, so a base-R-only restore would not actually restore draws.
- **Decision: `chk` validation incl. the NA/INT_MIN primer encoding.** `seed` whole number; `state` a length-2 integer vector that may contain `NA_integer_` (the §2 INT_MIN encoding dqrng accepts in `stream`); `.local_envir` an environment. Matches the legacy "Seed and state argument validation" requirement style. *Alternative*: reject `NA_integer_` — rejected; it is a valid, intentional primer value (§2).
- **Decision: optional `with_dqrng_state()` companion.** Provide it for parity with `with_lecuyer_cmrg_state()`, delegating to the `local_*` form, so existing call sites/tests have a familiar `with_*` shape. Low cost, keeps the two paths symmetric until `cleanup-lecuyer`.

## Risks / Trade-offs

- **dqrng state capture/restore may not be exact** → Mitigated by a test that takes draws, runs a nested `local_dqrng_state()` scope, and asserts the outer stream continues byte-identically to a no-nested-scope control. If dqrng exposes no faithful snapshot, fall back to capturing and reinstating the last `(seed, state)` the active scope was seeded with (documented in Open Questions).
- **Tests must run under the active backend** → `dqrng-init` scopes `register_methods()` to scenario execution; the state-wrapper tests must activate the backend (via the `dqrng-init` helpers with `on.exit(restore)`), or assert behaviour purely through dqrng draws, to avoid depending on base R RNG semantics.
- **Two coexisting state primitives (L'Ecuyer + dqrng)** → Accepted; intentional one-release overlap. Docs cross-reference both and mark dqrng as the path forward; `cleanup-lecuyer` removes the legacy one.
- **`.local_envir` misuse (wrong frame)** → Mitigated by the `parent.frame()` default and `chk::chk_environment()`, matching the legacy helper.

## Migration Plan

Additive and reversible. New `local_dqrng_state()` (+ optional `with_dqrng_state()`) and tests; new exports + `man/` pages. No existing function changes; the L'Ecuyer-CMRG helpers and the existing pipeline are untouched. No data migration. Removing the new file and its `NAMESPACE` entries fully reverts the change.

## Open Questions

- Exact dqrng state snapshot/restore mechanism: does the installed `dqrng` version expose a faithful get/set of the generator state, or must restore re-seed to a captured `(seed, state)`? Resolve during implementation; the spec only requires that the surrounding stream is undisturbed.
- Whether `with_dqrng_state()` is worth shipping now or deferred until a call site needs it — implement only if it reduces churn in `state-primitives`.
- Interaction with the scenario-scoped `register_methods()` from `dqrng-init`: confirm the wrapper is only ever called while the backend is active, and document that contract (likely guaranteed because tasks run inside scenario execution).
