# dqrng-backend Specification

## Purpose

Activate and restore the dqrng `pcg64` RNG backend for a scoped frame via the `withr`-style `local_dqrng_backend()` helper — the foundation of the targets-based per-task `dqrng` + hash RNG path (`TARGETS-DESIGN.md` §2). The capability routes base R's RNG functions through dqrng's pcg64 while a scope is open, restores the previous backend on exit (including on error), and enforces a reentrant nesting discipline so nested calls are no-ops and the RNG stream is identical with or without them. It also declares `dqrng` as a hard dependency.
## Requirements
### Requirement: Scoped dqrng pcg64 backend activation
The package SHALL provide `local_dqrng_backend(.local_envir = parent.frame())` that activates the dqrng backend for the duration of the calling frame: it SHALL set `dqRNGkind("pcg64")` and call `dqrng::register_methods()` so that base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64 while the scope is open.

#### Scenario: Backend active within the scope
- **WHEN** `local_dqrng_backend()` is called within a function and that function has not yet returned
- **THEN** base R RNG calls SHALL be served by dqrng's registered methods, and `RNGkind()` SHALL report the registered (user-supplied) methods, for the duration of the calling frame

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is activated
- **THEN** the package SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

### Requirement: Reset backend on scope exit
When the frame that activated the backend exits (normally or via error), ssdsims SHALL reset the base R RNG backend via `dqrng::restore_methods()`, leaving the session's RNG routing as it was before the scope was entered.

#### Scenario: Methods reset on scope exit
- **WHEN** the frame that activated the backend with `local_dqrng_backend()` exits
- **THEN** the backend SHALL be reset (`dqrng::restore_methods()`) and base R RNG functions SHALL no longer be routed through dqrng (`RNGkind()` SHALL report its pre-scope value)

### Requirement: withr-style local backend helper
ssdsims SHALL provide an exported `local_dqrng_backend()` helper that activates the dqrng `pcg64` backend and, following the withr convention (compare `withr::local_seed()`), defers the reset to the exit of `.local_envir`. Scenario execution and any test or script that touches the backend mid-session SHALL use this helper (or the internal `set_dqrng_backend()` paired with `on.exit(reset_dqrng_backend())`).

#### Scenario: Backend reset on scope exit
- **WHEN** `local_dqrng_backend()` is called within a function or block
- **THEN** the dqrng `pcg64` backend SHALL be active until `.local_envir` exits, at which point it SHALL be reset (including on error)

### Requirement: Reentrant nesting leaves the RNG stream unchanged
`local_dqrng_backend()` SHALL be reentrant: when called while the backend is already active, the call SHALL be a no-op (it SHALL NOT re-register the methods and SHALL NOT defer a further reset). Only the outermost call SHALL register on entry and reset on exit. Consequently the RNG stream SHALL be identical whether or not a nested `local_dqrng_backend()` call occurs.

#### Scenario: Nested call detected as no-op
- **WHEN** `local_dqrng_backend()` is called while a backend scope is already open (detected via `RNGkind()[1] == "user-supplied"`)
- **THEN** the nested call SHALL neither re-register methods nor schedule an additional reset, and the backend SHALL remain active until the outermost scope exits

#### Scenario: Stream identical with or without a nested call
- **WHEN** the same seeded draw sequence is taken once with an intervening nested `local_dqrng_backend()` call and once without it
- **THEN** the two draw sequences SHALL be identical

### Requirement: dqrng is a package dependency
The package SHALL declare `dqrng` in `Imports`.

#### Scenario: dqrng available at runtime
- **WHEN** ssdsims is installed
- **THEN** `dqrng` SHALL be installed as a hard dependency and available to the backend helpers

### Requirement: Reproducible draws under the dqrng backend
With the dqrng pcg64 backend active, seeding via `dqrng::dqset.seed(seed, stream)` SHALL produce a reproducible draw sequence for a given `(seed, stream)` pair.

#### Scenario: Same seed and stream reproduce draws
- **WHEN** `dqrng::dqset.seed(seed, stream)` is set to the same `(seed, stream)` on two occasions and the same draws are taken
- **THEN** the two draw sequences SHALL be identical

### Requirement: dqrng-specific backend integrity witness
The package SHALL provide an internal guard that verifies **dqrng specifically** — not merely *some* user-supplied RNG — currently holds base R's user-supplied RNG slot, and aborts otherwise. Because base R has a single process-global user-supplied RNG resolved by the `user_unif_rand` symbol across all loaded DLLs (to the most-recently-loaded match), a foreign user-RNG package can hold the slot while `RNGkind()[1]` still reports `"user-supplied"`; the existing `RNGkind()`-based `dqrng_backend_active()` probe SHALL NOT be relied on to distinguish dqrng from such a foreign generator (it remains the cheap reentrancy gate for `local_dqrng_backend()` only). The witness SHALL use dqrng's own state as evidence: record `dqrng::dqrng_get_state()`, take one base-R draw (which routes through the `user_unif_rand` slot), and re-read the state — the backend is intact if and only if dqrng's state advanced.

#### Scenario: Intact when dqrng holds the slot
- **WHEN** the dqrng backend is active and dqrng is the generator bound to base R's `user_unif_rand` slot, and the integrity witness is run
- **THEN** the recorded dqrng state SHALL have advanced after the base-R draw, and the guard SHALL return invisibly (the backend is intact)

#### Scenario: Aborts when a foreign user-RNG holds the slot
- **WHEN** a foreign user-supplied RNG (e.g. a second package such as `randtoolbox` loaded later) is bound to the `user_unif_rand` slot — so `RNGkind()[1]` still reports `"user-supplied"` — and the integrity witness is run
- **THEN** dqrng's recorded state SHALL be unchanged after the base-R draw (the draw advanced the foreign generator instead), and the guard SHALL abort with an informative error reported in the user-facing frame

#### Scenario: Abort message names the conflicting package
- **WHEN** the guard aborts because a foreign user-supplied RNG holds the slot
- **THEN** the error message SHALL name the package that currently owns the `user_unif_rand` symbol (resolved via `getNativeSymbolInfo("user_unif_rand")$dll[["name"]]`) and SHALL list the loaded packages that provide a user-supplied RNG (those among `getLoadedDLLs()` that export `user_unif_rand`), so the offending co-load is identified

#### Scenario: Abort message distinguishes a torn-down backend from a hijack
- **WHEN** the guard aborts because base R's RNG is no longer user-supplied at all (`RNGkind()[1] != "user-supplied"` — the backend was reset mid-task)
- **THEN** the error message SHALL report the current `RNGkind()` and that the dqrng backend was reset, and SHALL NOT name a symbol owner (which would be misleading, as the resolved symbol is not serving RNG)

#### Scenario: Witness is non-destructive
- **WHEN** the integrity witness runs (recording dqrng's state, drawing once, then restoring the recorded state via `dqrng::dqrng_set_state()`)
- **THEN** the dqrng draw sequence after the witness SHALL be identical to the sequence that would have been produced had the witness not run — the check SHALL consume no net randomness

