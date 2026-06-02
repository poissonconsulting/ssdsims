# parallel-safe-seeding Specification

## Purpose

Provide L'Ecuyer-CMRG-based RNG helpers that produce reproducible, non-overlapping random number streams across simulations and streams, so that parallel or distributed simulation runs yield deterministic results without statistical correlation between workers. The capability distinguishes a *seed* (a scalar integer consumed by `base::set.seed()`) from a *state* (the full length-7 `.Random.seed` vector for L'Ecuyer-CMRG), exposing scoped helpers for each.

## Requirements

### Requirement: Scoped L'Ecuyer-CMRG seeding from a scalar seed
The package SHALL expose `with_lecuyer_cmrg_seed(seed, code)` and `local_lecuyer_cmrg_seed(seed, .local_envir)` that pin the RNG kind to `L'Ecuyer-CMRG` with `Inversion` normal kind and `Rejection` sample kind and seed it from a scalar integer `seed`. `with_lecuyer_cmrg_seed()` SHALL forward to `local_lecuyer_cmrg_seed()`.

#### Scenario: with_lecuyer_cmrg_seed evaluates code
- **WHEN** `with_lecuyer_cmrg_seed(42, { runif(3) })` is called
- **THEN** the function SHALL set the RNG to L'Ecuyer-CMRG, seed it with `42`, evaluate the expression, return the result, and restore the previous RNG state

#### Scenario: local_lecuyer_cmrg_seed scopes to calling frame
- **WHEN** `local_lecuyer_cmrg_seed(seed)` is called inside a function
- **THEN** the L'Ecuyer-CMRG RNG state SHALL be active for the remainder of that function's execution and SHALL be restored when the function exits

#### Scenario: Reproducibility across calls
- **WHEN** `with_lecuyer_cmrg_seed(seed, code)` is invoked twice with the same `seed` and same `code`
- **THEN** the two invocations SHALL return identical results

### Requirement: Scoped L'Ecuyer-CMRG state installation from a state vector
The package SHALL expose `with_lecuyer_cmrg_state(state, code)` and `local_lecuyer_cmrg_state(state, .local_envir)` that temporarily install a `.Random.seed`-style length-7 L'Ecuyer-CMRG `state` vector (such as one produced by `parallel::nextRNGStream()`) by assigning to `.Random.seed`, then restore the previous state when the scope exits. `with_lecuyer_cmrg_state()` SHALL forward to `local_lecuyer_cmrg_state()`.

#### Scenario: Install and restore a stream state
- **WHEN** `local_lecuyer_cmrg_state(state)` is called inside a function with a length-7 L'Ecuyer-CMRG state vector
- **THEN** the RNG SHALL adopt that exact state for the remainder of the function and SHALL be restored to the prior state on exit

#### Scenario: Seeding then advancing a stream is reproducible
- **WHEN** a state is derived via `with_lecuyer_cmrg_seed(42, parallel::nextRNGStream(.Random.seed))` and then installed with `with_lecuyer_cmrg_state(state, runif(3))`
- **THEN** repeating the same two steps SHALL yield identical draws

### Requirement: Seed and state argument validation
The scoped helpers SHALL validate their arguments and abort with an informative error on invalid input.

#### Scenario: Non-whole-number seed
- **WHEN** `local_lecuyer_cmrg_seed()` or `with_lecuyer_cmrg_seed()` is called with a `seed` that is not a whole number
- **THEN** the function SHALL abort with an error

#### Scenario: Malformed state vector
- **WHEN** `local_lecuyer_cmrg_state()` or `with_lecuyer_cmrg_state()` is called with a `state` that is not a length-7 integer vector free of missing values
- **THEN** the function SHALL abort with an error

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
