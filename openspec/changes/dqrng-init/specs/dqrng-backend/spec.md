## ADDED Requirements

### Requirement: dqrng pcg64 backend at scenario execution start
At the start of scenario execution, ssdsims SHALL configure `dqrng` to use the `pcg64` generator and register dqrng as the backend for base R's random-number functions, so that `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64 during the scenario's execution.

#### Scenario: Backend active during scenario execution
- **WHEN** a scenario is executed (e.g., via `ssd_run_scenario()` or a dedicated `ssd_execute_scenario()`)
- **THEN** `dqRNGkind()` SHALL report `pcg64` and base R RNG calls SHALL be served by dqrng's registered methods for the duration of the execution

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is initialised at scenario start
- **THEN** ssdsims SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

### Requirement: Reset backend on scenario exit
On scenario exit, ssdsims SHALL reset the base R RNG backend via `dqrng::restore_methods()` so that the scenario's execution leaves the session's RNG routing as it was before the scenario started.

#### Scenario: Methods reset on scenario exit
- **WHEN** a scenario execution completes (normally or via error)
- **THEN** the backend SHALL be reset (`dqrng::restore_methods()`) and base R RNG functions SHALL no longer be routed through dqrng

### Requirement: withr-style local backend helper
ssdsims SHALL provide an exported `local_dqrng_backend()` helper that activates the dqrng `pcg64` backend and, following the withr convention (compare `withr::local_seed()`), defers the reset to the exit of `.local_envir`. Scenario execution and any test or script that touches the backend mid-session SHALL use this helper (or the internal `set_dqrng_backend()` paired with `on.exit(reset_dqrng_backend())`).

#### Scenario: Backend reset on scope exit
- **WHEN** `local_dqrng_backend()` is called within a function or block
- **THEN** the dqrng `pcg64` backend SHALL be active until `.local_envir` exits, at which point it SHALL be reset (including on error)

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
