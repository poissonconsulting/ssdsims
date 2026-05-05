# parallel-safe-seeding Specification

## Purpose

Provide L'Ecuyer-CMRG-based RNG helpers that produce reproducible, non-overlapping random number streams across simulations and streams, so that parallel or distributed simulation runs yield deterministic results without statistical correlation between workers.

## Requirements

### Requirement: Scoped L'Ecuyer-CMRG seeding
The package SHALL expose `with_lecuyer_cmrg_seed(seed, code)` and `local_lecuyer_cmrg_seed(seed, .local_envir)` wrappers around `withr::with_seed()` / `withr::local_seed()` that pin the RNG kind to `L'Ecuyer-CMRG` with `Inversion` normal kind and `Rejection` sample kind.

#### Scenario: with_lecuyer_cmrg_seed evaluates code
- **WHEN** `with_lecuyer_cmrg_seed(42, { runif(3) })` is called
- **THEN** the function SHALL set the RNG to L'Ecuyer-CMRG, seed it with `42`, evaluate the expression, return the result, and restore the previous RNG state

#### Scenario: local_lecuyer_cmrg_seed scopes to calling frame
- **WHEN** `local_lecuyer_cmrg_seed(seed)` is called inside a function
- **THEN** the L'Ecuyer-CMRG RNG state SHALL be active for the remainder of that function's execution and SHALL be restored when the function exits

#### Scenario: Reproducibility across calls
- **WHEN** `with_lecuyer_cmrg_seed(seed, code)` is invoked twice with the same `seed` and same `code`
- **THEN** the two invocations SHALL return identical results

### Requirement: Non-overlapping sub-streams for simulations
The package SHALL internally generate per-simulation RNG seeds by advancing L'Ecuyer-CMRG sub-streams so that `nsim` simulations within a single stream use statistically independent seeds.

#### Scenario: Different simulations produce different seeds
- **WHEN** per-simulation seeds are generated for `nsim > 1` within a fixed `stream` and `start_sim`
- **THEN** each simulation SHALL receive a distinct L'Ecuyer-CMRG sub-stream seed advanced via `parallel::nextRNGSubStream()`

### Requirement: Stream isolation
Distinct `stream` values SHALL produce non-overlapping top-level L'Ecuyer-CMRG streams via `parallel::nextRNGStream()`, enabling parallel execution without correlated RNG sequences.

#### Scenario: Different streams, same start_sim
- **WHEN** seeds are generated for the same `seed` and `start_sim` but different `stream` values
- **THEN** the resulting seeds SHALL correspond to different top-level L'Ecuyer-CMRG streams

### Requirement: Seed independence from caller RNG state
Seed-generating helpers SHALL not leave the caller's global RNG state modified on return, regardless of whether a `seed` argument is supplied.

#### Scenario: Global RNG restored
- **WHEN** per-simulation seeds are generated inside `ssd_sim_data()` or related functions
- **THEN** `.Random.seed` and `RNGkind()` in the caller's global environment SHALL be identical before and after seed generation

#### Scenario: NULL seed still reproducible per-stream
- **WHEN** seed generation is invoked with `seed = NULL`
- **THEN** the helper SHALL use the current global RNG state to seed L'Ecuyer-CMRG without persistently modifying it
