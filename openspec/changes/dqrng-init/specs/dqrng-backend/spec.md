## ADDED Requirements

### Requirement: dqrng pcg64 backend on load
On package load, ssdsims SHALL configure `dqrng` to use the `pcg64` generator and register dqrng as the backend for base R's random-number functions, so that `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64.

#### Scenario: Backend active after load
- **WHEN** the ssdsims package is loaded
- **THEN** `dqRNGkind()` SHALL report `pcg64` and base R RNG calls SHALL be served by dqrng's registered methods

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is initialised
- **THEN** ssdsims SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

### Requirement: Restore backend on unload
On package unload, ssdsims SHALL restore the previous (base R) RNG backend via `dqrng::restore_methods()` so that unloading the package leaves the session's RNG behaviour as it was before loading.

#### Scenario: Methods restored on unload
- **WHEN** the ssdsims package is unloaded
- **THEN** `dqrng::restore_methods()` SHALL be called and base R RNG functions SHALL no longer be routed through dqrng

### Requirement: dqrng is a package dependency
The package SHALL declare `dqrng` in `Imports`.

#### Scenario: dqrng available at runtime
- **WHEN** ssdsims is installed
- **THEN** `dqrng` SHALL be installed as a hard dependency and available to the load hook

### Requirement: Reproducible draws under the dqrng backend
With the dqrng pcg64 backend active, seeding via `dqrng::dqset.seed(seed, stream)` SHALL produce a reproducible draw sequence for a given `(seed, stream)` pair.

#### Scenario: Same seed and stream reproduce draws
- **WHEN** `dqrng::dqset.seed(seed, stream)` is set to the same `(seed, stream)` on two occasions and the same draws are taken
- **THEN** the two draw sequences SHALL be identical
