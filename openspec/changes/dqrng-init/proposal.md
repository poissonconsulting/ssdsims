## Why

The targets redesign replaces the L'Ecuyer-CMRG sub-stream lattice with a per-task `dqrng` + hash RNG mechanism (`TARGETS-DESIGN.md` §2), validated by `scripts/experiment-dqrng-hash.R`. Before any per-task primer or `_state` primitive can be built, the package needs `dqrng` available and configured as the active RNG backend. `dqrng-init` is a DAG root (no dependencies) in the §12 roadmap and the foundation of the entire dqrng RNG path.

## What Changes

- Add `dqrng` to `Imports` in `DESCRIPTION`.
- Add a `withr`-style `local_dqrng_backend(.local_envir = parent.frame())` helper that activates the dqrng backend for the duration of the calling frame: it sets `dqRNGkind("pcg64")`, calls `dqrng::register_methods()` so base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` (and therefore `dplyr::slice_sample()` and `ssdtools::ssd_r*()`) consume RNG via dqrng's pcg64, and defers `dqrng::restore_methods()` to restore the previous backend on exit.
- **Reentrancy (nesting guarantee).** `register_methods()` / `restore_methods()` keep a single global save-slot, so a nested `restore_methods()` would tear the backend down for the enclosing scope. `local_dqrng_backend()` is therefore made reentrant: on entry it detects an already-active backend (`RNGkind()[1] == "user-supplied"`) and, if active, is a **no-op** — it neither re-registers nor defers a restore. Only the outermost call registers on entry and restores on exit. This guarantees the RNG stream is **identical with or without a nested `local_dqrng_backend()` call** (verified empirically against dqrng 0.4.1).
- The choice of `pcg64` is forced: empirically only pcg64 and Threefry handle a length-2 `stream` argument without hanging (§2); pcg64 is selected for being well-tested, fast, and supporting streams by construction. The package overrides dqrng's own default (`Xoroshiro128++`).
- This step is **infrastructure only**: it adds the dependency and the backend wiring (activation, reentrant nesting, restore); it does **not** add primer derivation, `_state` primitives, or any public RNG API (those are later roadmap steps).

## Capabilities

### New Capabilities
- `dqrng-backend`: Activating and restoring the dqrng pcg64 RNG backend for a scoped frame via the `withr`-style `local_dqrng_backend()` helper, including the reentrant nesting discipline (nested calls are no-ops so the RNG stream is unchanged with or without them) and the `register_methods()` / `restore_methods()` routing of base R RNG through dqrng.

### Modified Capabilities
<!-- None at the spec level for this step. The existing parallel-safe-seeding
     capability (L'Ecuyer-CMRG) remains in place and unchanged; it is removed
     later by the cleanup-lecuyer roadmap step, not here. -->

## Impact

- **Dependencies**: New `Imports: dqrng` in `DESCRIPTION`.
- **New code**: `local_dqrng_backend()` plus a small internal predicate to detect an active backend (`RNGkind()[1] == "user-supplied"`), in `R/dqrng-backend.R` (or `R/zzz.R`).
- **Process-global side effect**: while a `local_dqrng_backend()` scope is open, `register_methods()` switches the base R RNG backend (§9 limitation); the reentrant design confines registration to the outermost scope and restores on its exit. Tests and helper scripts that touch the methods directly must observe the same restore discipline (prefer `local_dqrng_backend()`; otherwise `on.exit(restore_methods())`).
- **Verification**: `scripts/experiment-dqrng-hash.R` continues to pass.
- **Coexistence**: The L'Ecuyer-CMRG helpers and the existing pipeline are unaffected; both RNG backends are present until `cleanup-lecuyer`.
- **Downstream**: Unblocks `local-dqrng-state`, then `task-primer` and `state-primitives`.
