## Why

`local_dqrng_state()` / `with_dqrng_state()` (landed, #78) take an argument named `state` that actually holds the per-task **primer** (the value passed to dqrng's `stream`). `GLOSSARY.md` flags this as "a historical misnomer, to be fixed in a future iteration: `primer =` argument of the `_primer` functions." The new `primer-primitives` work adopts `primer` naming; this micro-change settles the landed helper's argument to match, so the whole dqrng path speaks "primer" for that value. Scope is deliberately tiny: rename one argument.

## What Changes

- Rename the `state` argument → `primer` in `local_dqrng_state(seed, primer, .local_envir = parent.frame())` and `with_dqrng_state(seed, primer, code)`, updating the body (`dqrng::dqset.seed(seed, stream = primer)`, the `chk_*` calls, `invisible(primer)`), the roxygen (`@param primer`, `@return`, prose), and any call sites.
- The function **names** stay `local_dqrng_state()` / `with_dqrng_state()` — the `_state` there is legitimate (these `withr`-style helpers scope the real dqrng generator *state*, snapshotting/restoring it via `dqrng_get_state()`/`dqrng_set_state()`). Only the *argument*, which carries a primer, is the misnomer.
- The internal `get_dqrng_state()` / `set_dqrng_state()` helpers (which handle the actual generator state) are **unchanged**.

## Capabilities

### New Capabilities
<!-- None. -->

### Modified Capabilities
- `parallel-safe-seeding`: the dqrng state-installation helper's argument is named `primer` (not `state`), matching the GLOSSARY and the `primer-primitives` naming.

## Impact

- **Code**: `R/dqrng-state.R` (argument rename in both functions + roxygen); `tests/testthat/test-dqrng-state.R` (any named `state =` calls); regenerated `man/local_dqrng_state.Rd`.
- **APIs**: `local_dqrng_state()` / `with_dqrng_state()` are exported; renaming a named argument is a minor breaking change. No in-package callers pass `state =` (only `primer-primitives`, not yet implemented, will call these — it already uses `primer`). The pre-release status (`TARGETS-DESIGN.md` §12: breaking steps are fine) covers it.
- **Dependencies**: none. Derived from `primer-primitives`; can land independently and before it.
- **Downstream**: `primer-primitives` calls `local_dqrng_state(seed, primer)` directly (no `state = primer` adapter needed once this lands). Closes the GLOSSARY "historical misnomer" note for this helper.
