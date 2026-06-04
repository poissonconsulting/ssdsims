## MODIFIED Requirements

### Requirement: dqrng is a package dependency
The package SHALL declare `dqrng` (`>= 0.4.1`) in `Suggests`, **not** `Imports`, and SHALL NOT load `dqrng` implicitly. Loading a package that registers a user-supplied RNG is a process-global, potentially destructive act (it places a `user_unif_rand` provider into the session, which can collide with another user-RNG package), so ssdsims SHALL use `dqrng` only when the caller has already loaded it. All `dqrng` usage SHALL be gated on `dqrng` being **already loaded** at the required version — tested via `isNamespaceLoaded("dqrng")` together with `getNamespaceVersion("dqrng") >= "0.4.1"` — and SHALL NOT use `requireNamespace("dqrng")` or a bare `dqrng::` call as the gate, because those would themselves load `dqrng`.

#### Scenario: dqrng is suggested, not imported
- **WHEN** ssdsims is installed
- **THEN** `dqrng` SHALL appear in `Suggests` (`>= 0.4.1`) and SHALL NOT appear in `Imports`, so installing or loading ssdsims does not load `dqrng`

#### Scenario: dqrng is not loaded implicitly by ssdsims
- **WHEN** ssdsims is loaded and no scenario has been run, and the caller has not loaded `dqrng`
- **THEN** `dqrng` SHALL NOT be among the loaded namespaces (ssdsims SHALL NOT have triggered its load)

### Requirement: Scoped dqrng pcg64 backend activation
The package SHALL provide `local_dqrng_backend(.local_envir = parent.frame())` that activates the dqrng backend for the duration of the calling frame: it SHALL set `dqRNGkind("pcg64")` and call `dqrng::register_methods()` so that base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64 while the scope is open. Activation SHALL be conditional on `dqrng` being already loaded at the required version (per *dqrng is a package dependency*): when it is not, the helper SHALL abort with an actionable error instructing the caller to load `dqrng` (`>= 0.4.1`), and SHALL NOT load `dqrng` itself.

#### Scenario: Backend active within the scope
- **WHEN** `local_dqrng_backend()` is called within a function (with `dqrng` already loaded at `>= 0.4.1`) and that function has not yet returned
- **THEN** base R RNG calls SHALL be served by dqrng's registered methods, and `RNGkind()` SHALL report the registered (user-supplied) methods, for the duration of the calling frame

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is activated
- **THEN** the package SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

#### Scenario: Aborts when dqrng is not already loaded
- **WHEN** backend activation is requested but `dqrng` is not loaded (or its loaded version is `< 0.4.1`)
- **THEN** the helper SHALL abort with an informative error directing the caller to run `library(dqrng)` (`>= 0.4.1`), SHALL NOT load `dqrng`, and SHALL NOT fall back to base R's RNG

## ADDED Requirements

### Requirement: dqrng-specific backend integrity witness
The package SHALL provide an internal guard that verifies **dqrng specifically** — not merely *some* user-supplied RNG — currently holds base R's user-supplied RNG slot, and aborts otherwise. Because base R has a single process-global user-supplied RNG resolved by the `user_unif_rand` symbol across all loaded DLLs, a foreign user-RNG package can hold the slot while `RNGkind()[1]` still reports `"user-supplied"`; the existing `RNGkind()`-based `dqrng_backend_active()` probe SHALL NOT be relied on to distinguish dqrng from such a foreign generator. The witness SHALL use dqrng's own state as evidence: record `dqrng::dqrng_get_state()`, take one base-R draw (which routes through the `user_unif_rand` slot), and re-read the state — the backend is intact if and only if dqrng's state advanced.

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
