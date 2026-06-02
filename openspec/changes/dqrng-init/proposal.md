## Why

The targets redesign replaces the L'Ecuyer-CMRG sub-stream lattice with a per-task `dqrng` + hash RNG mechanism (`TARGETS-DESIGN.md` §2), validated by `scripts/experiment-dqrng-hash.R`. Before any per-task primer or `_state` primitive can be built, the package needs `dqrng` available and configured as the active RNG backend. `dqrng-init` is a DAG root (no dependencies) in the §12 roadmap and the foundation of the entire dqrng RNG path.

## What Changes

- Add `dqrng` to `Imports` in `DESCRIPTION`.
- On package load, set `dqRNGkind("pcg64")` and call `dqrng::register_methods()` so base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` (and therefore `dplyr::slice_sample()` and `ssdtools::ssd_r*()`) consume RNG via dqrng's pcg64.
- On package unload, call `dqrng::restore_methods()` to restore the previous (base R) RNG backend.
- The choice of `pcg64` is forced: empirically only pcg64 and Threefry handle a length-2 `stream` argument without hanging (§2); pcg64 is selected for being well-tested, fast, and supporting streams by construction. The package overrides dqrng's own default (`Xoroshiro128++`).
- This step is **infrastructure only**: it adds the dependency and the backend wiring; it does **not** add primer derivation, `_state` primitives, or any public RNG API (those are later roadmap steps).

## Capabilities

### New Capabilities
- `dqrng-backend`: Initialising and tearing down the process-global dqrng pcg64 RNG backend on package load/unload, including the `register_methods()` / `restore_methods()` discipline that routes base R RNG through dqrng.

### Modified Capabilities
<!-- None at the spec level for this step. The existing parallel-safe-seeding
     capability (L'Ecuyer-CMRG) remains in place and unchanged; it is removed
     later by the cleanup-lecuyer roadmap step, not here. -->

## Impact

- **Dependencies**: New `Imports: dqrng` in `DESCRIPTION`.
- **New code**: `.onLoad()` / `.onUnload()` hooks in `R/ssdsims-package.R` (or a dedicated `R/zzz.R`); a small internal helper to set/restore the backend.
- **Process-global side effect**: `register_methods()` switches the base R RNG backend for the whole session (§9 limitation). Tests and helper scripts touching the methods must observe the same restore discipline (`on.exit(restore_methods())`).
- **Verification**: `scripts/experiment-dqrng-hash.R` continues to pass.
- **Coexistence**: The L'Ecuyer-CMRG helpers and the existing pipeline are unaffected; both RNG backends are present until `cleanup-lecuyer`.
- **Downstream**: Unblocks `local-dqrng-state`, then `task-primer` and `state-primitives`.
