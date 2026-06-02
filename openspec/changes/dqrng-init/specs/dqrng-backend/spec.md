## ADDED Requirements

### Requirement: dqrng pcg64 backend at scenario execution start
At the start of scenario execution, ssdsims SHALL configure `dqrng` to use the `pcg64` generator and register dqrng as the backend for base R's random-number functions, so that `runif()`, `rnorm()`, `rbinom()`, `rexp()`, `rgamma()`, `rpois()`, `sample.int()`, and `sample()` draw from dqrng's pcg64 during the scenario's execution.

#### Scenario: Backend active during scenario execution
- **WHEN** a scenario is executed (e.g., via `ssd_run_scenario()` or a dedicated `ssd_execute_scenario()`)
- **THEN** `dqRNGkind()` SHALL report `pcg64` and base R RNG calls SHALL be served by dqrng's registered methods for the duration of the execution

#### Scenario: pcg64 chosen over the dqrng default
- **WHEN** the backend is initialised at scenario start
- **THEN** ssdsims SHALL explicitly set `pcg64`, overriding dqrng's own default generator (`Xoroshiro128++`)

### Requirement: Restore backend on scenario exit
On scenario exit, ssdsims SHALL restore the previous (base R) RNG backend via `dqrng::restore_methods()` so that the scenario's execution leaves the session's RNG behaviour as it was before the scenario started.

#### Scenario: Methods restored on scenario exit
- **WHEN** a scenario execution completes (normally or via error)
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
