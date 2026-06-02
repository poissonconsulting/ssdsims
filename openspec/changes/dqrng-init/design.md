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
- **Decision: scope registration to scenario execution, not process-global.** The scenario runner (later merged into `ssd_run_scenario()` or a dedicated `ssd_execute_scenario()`) activates the backend on entry and resets it on exit. This avoids surprising global side effects in the session and simplifies cleanup — each scenario is responsible for its own RNG backend. *Alternative*: process-global on `.onLoad()` — rejected; couples the package's default behaviour to the caller's session and requires `.onUnload()` symmetry, which is fragile in interactive use and testing.
- **Decision: withr-style `local_dqrng_backend()`, `set`/`reset` naming.** The backend lives in `R/dqrng-backend.R` (not `zzz.R`, which conventionally holds `.onLoad`/`.onUnload` hooks that this design deliberately omits). Two internal primitives — `set_dqrng_backend()` (`dqRNGkind("pcg64")` + `register_methods()`) and `reset_dqrng_backend()` (`restore_methods()`) — are wrapped by an exported `local_dqrng_backend(.local_envir = parent.frame())` that defers the reset to the calling frame's exit, mirroring `withr::local_seed()`. We use **reset** (not *restore*) to match the withr `set`/`reset` pairing. `run_scenario()` calls `local_dqrng_backend()` on entry. *Alternative*: a bare `set` + `on.exit(reset)` at every call site — kept available, but `local_dqrng_backend()` is the preferred, less error-prone entry point.
- **Decision: make `local_dqrng_backend()` reentrant (nesting must not move the stream).** Empirically (dqrng 0.4.1) `register_methods()`/`restore_methods()` keep a *single* global save-slot: a nested `reset_dqrng_backend()` reverts to base R for the still-open outer scope, so naive nesting corrupts the stream (verified: nested ≠ non-nested draws). The fix is reentrancy — on entry, if ssdsims already has the backend active, return a no-op (no re-`set`, no deferred `reset`). Only the outermost scope registers and resets. Verified: with this rule the draw sequence is identical with or without an intervening nested call. (This is a follow-up on top of the already-implemented backend; see tasks §5.)
- **Decision: track activation with package-private state, not a `RNGkind()` probe.** The initial implementation inferred "backend active" from `RNGkind()[1] == "user-supplied"`. That is ambiguous: *any* user-supplied RNG backend (not just dqrng) reports `"user-supplied"`, so a foreign backend installed by the caller would make `local_dqrng_backend()` wrongly no-op and fail to activate dqrng (raised in Copilot review of #74). Instead, `set_dqrng_backend()` sets and `reset_dqrng_backend()` clears a package-private flag, and `dqrng_backend_active()` reads it — tracking *our own* activation precisely. *Alternative*: the `RNGkind()` probe — rejected for the false-positive above. *Alternative*: query dqrng directly — not possible; dqrng 0.4.1 exposes no "are my methods registered?" getter (`dqRNGkind()` is a setter only). Faithful generator-state snapshot/restore, when a caller needs it, is available via `dqrng_get_state()` / `dqrng_set_state()` (the `local-dqrng-state` step uses these and pins `dqrng (>= 0.4.0)`).

## Risks / Trade-offs

- **Callers must consistently restore on exit** → `local_dqrng_backend()` defers the restore to `.local_envir`, so correct use is automatic; direct `register_methods()` callers in tests/scripts must guard with `on.exit(restore_methods())`. Parallel workers (targets + dynamic branching) inherit the parent's restored state at entry.
- **Nested activation could silently corrupt the stream** → Addressed by the reentrant no-op design (a nested call touches nothing); a regression test asserts the draw sequence is identical with vs. without a nested `local_dqrng_backend()` call.
- **dqrng version drift changes draw sequences** → Pin `dqrng` version in the scenario manifest (§8.5); `scripts/experiment-dqrng-hash.R` acts as a regression guard.
- **Tests that assume base R RNG semantics may shift** → `dqrng::register_methods()` advances base R's `.Random.seed` as a side effect (`dqRNGkind()` does not), so activating the backend shifts the base R RNG stream. The audit found one brittle test (an *unseeded* `run_scenario()` coef snapshot) whose value depended on ambient RNG ordering; it is pinned with `with_lecuyer_cmrg_seed()` in a separate, forward-portable commit so its data sampling — and therefore its fitted coefficients (which are RNG-independent given fixed data) — are deterministic. With that in place, enabling the backend changes no snapshots.

## Migration Plan

Additive and reversible at the scope level. Package load is inert (no RNG backend change); each `local_dqrng_backend()` scope activates dqrng and restores on exit. No data migration. The L'Ecuyer-CMRG path remains fully functional alongside dqrng until `cleanup-lecuyer`.

## Open Questions

- Do any current tests rely on base R's default RNG sequence such that the backend switch (during scenario execution) changes snapshots? **Resolved:** one unseeded `run_scenario()` coef test was brittle to RNG-stream shifts; it is now seed-pinned (separate commit), after which the backend changes no snapshots.
- ~~What if a scenario is nested (one scenario spawns another)?~~ **Resolved**: `local_dqrng_backend()` is made reentrant — a nested scope is a no-op and the outer backend stays active, so nesting is transparent to the RNG stream (verified against dqrng 0.4.1; see tasks §5).
- Threefry remains unreviewed (§2); no action now, but note it as a future alternative generator.
