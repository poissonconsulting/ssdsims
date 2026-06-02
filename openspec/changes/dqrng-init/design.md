## Context

`TARGETS-DESIGN.md` §2 replaces the L'Ecuyer-CMRG sub-stream lattice with per-task `dqrng` + hash seeding, validated end-to-end by `scripts/experiment-dqrng-hash.R`. This change lands only the *backend initialisation* — the prerequisite for `local-dqrng-state`, `task-primer`, and `state-primitives`. It is a §12 DAG root with no dependencies.

The §9 limitation "`dqrng::register_methods()` is process-global" is the central constraint: switching the base R RNG backend affects the whole session, so the load/unload discipline must be symmetric.

## Goals / Non-Goals

**Goals**
- Add `dqrng` to `Imports`.
- Set `dqRNGkind("pcg64")` and `register_methods()` on load; `restore_methods()` on unload.
- Keep `scripts/experiment-dqrng-hash.R` passing.

**Non-Goals**
- Per-task primer derivation (`task-primer`).
- `local_dqrng_state()` wrapper (`local-dqrng-state`).
- Refactoring `slice_sample_state()` / `fit_dists_state()` / `hc_state()` onto dqrng (`state-primitives`).
- Removing the L'Ecuyer-CMRG helpers (`cleanup-lecuyer`).

## Decisions

- **Decision: pcg64, set explicitly.** §2 records that Xoroshiro128++/Xoshiro256++ hang on a length-2 `stream` argument; only pcg64 and Threefry survive. pcg64 is well-tested, fast, and supports streams by construction (each stream is a distinct LCG increment ⇒ independent sequences). Threefry is not yet reviewed. *Alternative*: leave dqrng's default (Xoroshiro128++) — rejected, it cannot take the length-2 stream this design relies on.
- **Decision: register base R methods globally.** `register_methods()` routes `runif`/`sample.int`/etc. — and therefore `dplyr::slice_sample()` and `ssdtools::ssd_r*()` — through dqrng. This is what lets the (state-less) inner ops consume the per-task RNG without a `state=` argument (§2, `state-primitives`). *Alternative*: call dqrng functions directly at every draw site — rejected; it would require forking ssdtools' internals.
- **Decision: symmetric load/unload hooks.** `.onLoad()` sets kind + registers; `.onUnload()` restores. Helper scripts/tests that touch the methods mid-session follow the same `on.exit(restore_methods())` discipline (documented §9). *Alternative*: lazy/first-use init — rejected; less predictable and harder to reason about in parallel workers.
- **Decision: place hooks in `R/zzz.R`.** Conventional R location for `.onLoad`/`.onUnload`.

## Risks / Trade-offs

- **Process-global backend switch surprises other code in the session** → Document in package docs and the manifest (§9); provide symmetric restore on unload; keep the switch explicit and observable via `dqRNGkind()`.
- **dqrng version drift changes draw sequences** → Pin `dqrng` version in the scenario manifest (§8.5); `scripts/experiment-dqrng-hash.R` acts as a regression guard.
- **Tests that assume base R RNG semantics may shift** → Audit RNG-touching tests when this lands; this step itself adds no RNG-consuming logic, limiting blast radius.

## Migration Plan

Additive and reversible within a session. Loading ssdsims switches the backend; unloading restores it. No data migration. The L'Ecuyer-CMRG path remains fully functional alongside dqrng until `cleanup-lecuyer`.

## Open Questions

- Should `.onLoad()` guard against re-registration if dqrng methods are already registered by another package in the session?
- Do any current tests rely on base R's default RNG sequence such that the backend switch changes snapshots? (Audit during implementation.)
- Threefry remains unreviewed (§2); no action now, but note it as a future alternative generator.
