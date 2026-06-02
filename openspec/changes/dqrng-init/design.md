## Context

`TARGETS-DESIGN.md` §2 replaces the L'Ecuyer-CMRG sub-stream lattice with per-task `dqrng` + hash seeding, validated end-to-end by `scripts/experiment-dqrng-hash.R`. This change lands only the *dependency and availability* — the prerequisite for `local-dqrng-state`, `task-primer`, and `state-primitives`. It is a §12 DAG root with no dependencies.

Key decision: `dqrng::register_methods()` is scoped to the duration of the scenario run (not process-global on package load). This eliminates surprising global side effects and simplifies cleanup: each scenario execution is responsible for activating dqrng at init and restoring on exit.

## Goals / Non-Goals

**Goals**
- Add `dqrng` to `Imports`.
- Configure `dqRNGkind("pcg64")` and register dqrng methods at scenario execution start; restore on exit.
- Keep `scripts/experiment-dqrng-hash.R` passing.

**Non-Goals**
- Per-task primer derivation (`task-primer`).
- `local_dqrng_state()` wrapper (`local-dqrng-state`).
- Refactoring `slice_sample_state()` / `fit_dists_state()` / `hc_state()` onto dqrng (`state-primitives`).
- Removing the L'Ecuyer-CMRG helpers (`cleanup-lecuyer`).

## Decisions

- **Decision: pcg64, set explicitly.** §2 records that Xoroshiro128++/Xoshiro256++ hang on a length-2 `stream` argument; only pcg64 and Threefry survive. pcg64 is well-tested, fast, and supports streams by construction (each stream is a distinct LCG increment ⇒ independent sequences). Threefry is not yet reviewed. *Alternative*: leave dqrng's default (Xoroshiro128++) — rejected, it cannot take the length-2 stream this design relies on.
- **Decision: register base R methods globally.** `register_methods()` routes `runif`/`sample.int`/etc. — and therefore `dplyr::slice_sample()` and `ssdtools::ssd_r*()` — through dqrng. This is what lets the (state-less) inner ops consume the per-task RNG without a `state=` argument (§2, `state-primitives`). *Alternative*: call dqrng functions directly at every draw site — rejected; it would require forking ssdtools' internals.
- **Decision: scope registration via a `withr`-style `local_dqrng_backend()`, not process-global.** Activation is bound to a calling frame: `local_dqrng_backend(.local_envir = parent.frame())` sets `dqRNGkind("pcg64")`, calls `register_methods()`, and `withr::defer(restore_methods(), envir = .local_envir)`. The scenario runner opens one such scope; tests open their own. This avoids surprising global side effects and makes cleanup automatic. *Alternative*: process-global on `.onLoad()` — rejected; couples the package's default behaviour to the caller's session and requires fragile `.onUnload()` symmetry. *Alternative*: bare `set_/restore_` pair with manual `on.exit()` — subsumed by the `local_*` form, which is the preferred idiom (CLAUDE.md §RNG discipline).
- **Decision: make `local_dqrng_backend()` reentrant (nesting must not move the stream).** Empirically (dqrng 0.4.1) `register_methods()`/`restore_methods()` keep a *single* global save-slot: a nested `restore_methods()` reverts to base R for the still-open outer scope, so naive nesting corrupts the stream (verified: nested ≠ non-nested draws). The fix is reentrancy — on entry, detect an active backend via `RNGkind()[1] == "user-supplied"`; if active, return a no-op (no re-register, no deferred restore). Only the outermost scope registers and restores. Verified: with this rule the draw sequence is identical with or without an intervening nested call. *Alternative*: a depth counter — equivalent but needs package-level mutable state; the `RNGkind()` probe is stateless and sufficient. *Alternative*: stack the saved methods ourselves — rejected; dqrng does not expose the slot and re-implementing it is fragile.
- **Decision: query backend state via `RNGkind()`, not `dqRNGkind()`.** dqrng 0.4.1 exposes no getter for the active generator (`dqRNGkind()` requires its `kind` argument); the observable signal that the backend is active is `RNGkind()[1] == "user-supplied"`. Faithful generator-state snapshot/restore, when needed by callers, is available via `dqrng_get_state()` / `dqrng_set_state()` (≥ 0.4.0). *Alternative*: assume `dqRNGkind()` is a getter — rejected; it is a setter only.

## Risks / Trade-offs

- **Callers must consistently restore on exit** → `local_dqrng_backend()` defers the restore to `.local_envir`, so correct use is automatic; direct `register_methods()` callers in tests/scripts must guard with `on.exit(restore_methods())`. Parallel workers (targets + dynamic branching) inherit the parent's restored state at entry.
- **Nested activation could silently corrupt the stream** → Addressed by the reentrant no-op design (a nested call touches nothing); a regression test asserts the draw sequence is identical with vs. without a nested `local_dqrng_backend()` call.
- **dqrng version drift changes draw sequences** → Pin `dqrng` version in the scenario manifest (§8.5); `scripts/experiment-dqrng-hash.R` acts as a regression guard.
- **Tests that assume base R RNG semantics may shift** → Audit RNG-touching tests when this lands; this step itself adds no RNG-consuming logic, limiting blast radius.

## Migration Plan

Additive and reversible at the scope level. Package load is inert (no RNG backend change); each `local_dqrng_backend()` scope activates dqrng and restores on exit. No data migration. The L'Ecuyer-CMRG path remains fully functional alongside dqrng until `cleanup-lecuyer`.

## Open Questions

- Do any current tests rely on base R's default RNG sequence such that the backend switch (during scenario execution) changes snapshots? (Audit during implementation.)
- ~~What if a scenario is nested (one scenario spawns another)?~~ **Resolved**: `local_dqrng_backend()` is reentrant — a nested scope is a no-op and the outer backend stays active, so nesting is transparent to the RNG stream (verified against dqrng 0.4.1).
- Threefry remains unreviewed (§2); no action now, but note it as a future alternative generator.
