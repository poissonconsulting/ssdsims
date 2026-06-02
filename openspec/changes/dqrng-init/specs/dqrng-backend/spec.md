## ADDED Requirements

### Requirement: Scoped dqrng pcg64 backend activation
The package SHALL provide `local_dqrng_backend(.local_envir = parent.frame())` that activates the dqrng backend for the duration of the calling frame: it SHALL set `dqRNGkind("pcg64")` and call `dqrng::register_methods()` so that base R's `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64 while the scope is open.

#### Scenario: Backend active within the scope
- **WHEN** `local_dqrng_backend()` is called within a function and that function has not yet returned
- **THEN** base R RNG calls SHALL be served by dqrng's registered methods, and `RNGkind()` SHALL report the registered (user-supplied) methods, for the duration of the calling frame

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is activated
- **THEN** the package SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

### Requirement: Restore backend on scope exit
When the frame that activated the backend exits (normally or via error), the package SHALL restore the previous (base R) RNG backend via `dqrng::restore_methods()`, leaving the session's RNG behaviour as it was before the scope was entered.

#### Scenario: Methods restored on scope exit
- **WHEN** the frame that activated the backend with `local_dqrng_backend()` exits
- **THEN** `dqrng::restore_methods()` SHALL be called and base R RNG functions SHALL no longer be routed through dqrng (`RNGkind()` SHALL report its pre-scope value)

### Requirement: Reentrant nesting leaves the RNG stream unchanged
`local_dqrng_backend()` SHALL be reentrant: when called while the backend is already active, the call SHALL be a no-op (it SHALL NOT re-register the methods and SHALL NOT defer a further restore). Only the outermost call SHALL register on entry and restore on exit. Consequently the RNG stream SHALL be identical whether or not a nested `local_dqrng_backend()` call occurs.

#### Scenario: Nested call detected as no-op
- **WHEN** `local_dqrng_backend()` is called while a backend scope is already open (detected via `RNGkind()[1] == "user-supplied"`)
- **THEN** the nested call SHALL neither re-register methods nor schedule an additional `restore_methods()`, and the backend SHALL remain active until the outermost scope exits

#### Scenario: Stream identical with or without a nested call
- **WHEN** the same seeded draw sequence is taken once with an intervening nested `local_dqrng_backend()` call and once without it
- **THEN** the two draw sequences SHALL be identical

### Requirement: dqrng is a package dependency
The package SHALL declare `dqrng (>= 0.4.0)` in `Imports`.

#### Scenario: dqrng available at runtime
- **WHEN** ssdsims is installed
- **THEN** `dqrng` (at least version 0.4.0, providing `dqrng_get_state()` / `dqrng_set_state()`) SHALL be installed as a hard dependency and available to the backend helper

### Requirement: Reproducible draws under the dqrng backend
With the dqrng pcg64 backend active, seeding via `dqrng::dqset.seed(seed, stream)` SHALL produce a reproducible draw sequence for a given `(seed, stream)` pair.

#### Scenario: Same seed and stream reproduce draws
- **WHEN** `dqrng::dqset.seed(seed, stream)` is set to the same `(seed, stream)` on two occasions and the same draws are taken
- **THEN** the two draw sequences SHALL be identical
