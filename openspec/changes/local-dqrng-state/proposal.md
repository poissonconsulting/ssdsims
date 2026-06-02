## Why

`dqrng-init` (now landed, a DAG root) makes the `dqrng` pcg64 backend available and registers it during scenario execution (`TARGETS-DESIGN.md` §2). The next thing the dqrng RNG path needs is a **scoped seeding primitive**: a `withr`-style wrapper that installs a per-task starting point and restores the prior RNG state on exit. This mirrors the legacy `local_lecuyer_cmrg_state()` but for the dqrng path, and is the prerequisite for `task-primer` (which derives the primer) and `state-primitives` (where each per-task body calls this wrapper exactly once). It is the roadmap entry on the `dqrng-init → local-dqrng-state` edge in the §12 DAG, now unblocked. Per CLAUDE.md's RNG discipline, we prefer `local_*` over `with_*` when touching code, so this is the primitive the rest of the dqrng path builds on.

## What Changes

- Add `local_dqrng_state(seed, state, .local_envir = parent.frame())`: a thin wrapper around `dqrng::dqset.seed(seed, stream = state)` that installs the `(seed, state)` starting point and registers a `withr`-style deferred restore of the prior RNG state when `.local_envir` exits (`TARGETS-DESIGN.md` §2; GLOSSARY: the value passed to dqrng's `stream` argument is a *primer*, and the `_state` suffix marks the wrapper that installs it as the running state).
- The wrapper captures the RNG state on entry (via `dqrng::dqrng_get_state()`) and restores it on exit (via `dqrng::dqrng_set_state()`) under `withr::defer()`, so a call leaves the surrounding RNG stream undisturbed — the same restore contract `local_lecuyer_cmrg_state()` provides for the legacy path. (The faithful state round-trip is confirmed against dqrng 0.4.1.)
- **Backend guard.** `local_dqrng_state()` SHALL **abort** if the dqrng backend is not in effect, using the `dqrng_backend_active()` helper provided by `dqrng-init` (rather than re-checking `RNGkind()` inline). The error directs the caller to open a `local_dqrng_backend()` scope first. This makes the dependency on the active backend explicit and fail-fast rather than silently seeding the wrong generator.
- Validate inputs with `chk` (whole-number `seed`; `state`/primer is a length-2 integer vector per §2, with `NA_integer_` permitted as the INT_MIN encoding; `.local_envir` is an environment).
- Optionally add a `with_dqrng_state(seed, state, code)` companion for symmetry with the legacy `with_*` helpers, delegating to `local_dqrng_state()`.
- **Scope guards (explicit non-goals):** this step does **not** derive the primer from a task hash (that is `task-primer`), does **not** refactor `slice_sample_state()` / `fit_dists_state()` / `hc_state()` onto dqrng (that is `state-primitives`), and does **not** remove the L'Ecuyer-CMRG helpers (that is `cleanup-lecuyer`). The legacy `local_lecuyer_cmrg_state()` stays in place alongside this one.

## Capabilities

### Modified Capabilities
- `parallel-safe-seeding`: Adds the dqrng-path scoped seeding primitive `local_dqrng_state()` (and optional `with_dqrng_state()`) alongside the existing L'Ecuyer-CMRG seed/state helpers. The capability already documents L'Ecuyer-CMRG and the dqrng seed/state helpers as its scope; this adds the dqrng `local_*` state wrapper with its backend guard and restore-on-exit guarantee. It builds on the reentrant `local_dqrng_backend()` from `dqrng-init`.

## Impact

- **New code**: `local_dqrng_state()` (and optional `with_dqrng_state()`) plus internal get/set-state helpers for dqrng, in `R/dqrng-state.R` (or co-located with the dqrng backend wiring from `dqrng-init`); `tests/testthat/test-dqrng-state.R`.
- **APIs**: New export `local_dqrng_state()` (and optional `with_dqrng_state()`); new `NAMESPACE` entries and roxygen `man/` pages, cross-referencing `withr::local_seed()` and `local_lecuyer_cmrg_state()`.
- **Dependencies**: None added — `dqrng` (from `dqrng-init`), `withr`, and `chk` are already in `Imports`.
- **RNG discipline**: The wrapper must restore RNG state on exit (CLAUDE.md §RNG discipline) via `dqrng_get_state()`/`dqrng_set_state()`; tested by asserting the surrounding stream is unchanged across a `local_dqrng_state()` scope, mirroring the L'Ecuyer-CMRG tests.
- **Depends on**: `local_dqrng_backend()` from `dqrng-init` — `local_dqrng_state()` is only valid inside an active backend scope and aborts otherwise.
- **Downstream**: Unblocks `task-primer` (derives the `state`/primer fed to this wrapper) and, with it, `state-primitives`.
