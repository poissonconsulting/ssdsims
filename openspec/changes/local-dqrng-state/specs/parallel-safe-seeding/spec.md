## ADDED Requirements

### Requirement: Scoped dqrng state installation from a (seed, primer) pair
The package SHALL provide `local_dqrng_state(seed, state, .local_envir = parent.frame())`, a thin wrapper around `dqrng::dqset.seed(seed, stream = state)` that installs the `(seed, state)` starting point as the running RNG state and registers a deferred restore of the prior RNG state when `.local_envir` exits. The `state` argument carries the per-task *primer* (the value passed to dqrng's `stream` argument, per `TARGETS-DESIGN.md` §2 and GLOSSARY). The package MAY additionally provide a `with_dqrng_state(seed, state, code)` companion that delegates to `local_dqrng_state()` and evaluates `code`.

#### Scenario: Install and restore dqrng state
- **WHEN** `local_dqrng_state(seed, state)` is called within a function and that function returns
- **THEN** the RNG state SHALL be set to `(seed, state)` for the duration of the calling frame and SHALL be restored to its prior value when the frame exits

#### Scenario: Same seed and primer reproduce draws
- **WHEN** `local_dqrng_state(seed, state)` is called twice with the same `(seed, state)` and the same draws are taken under the active dqrng backend
- **THEN** the two draw sequences SHALL be identical

#### Scenario: Surrounding RNG stream undisturbed
- **WHEN** code takes RNG draws, calls `local_dqrng_state(seed, state)` in a nested scope that itself takes draws, and then continues
- **THEN** after the nested scope exits the outer RNG stream SHALL continue as if the nested `local_dqrng_state()` scope had not consumed any RNG

### Requirement: dqrng state argument validation
`local_dqrng_state()` (and `with_dqrng_state()` if provided) SHALL validate its arguments: `seed` SHALL be a whole number, `state` SHALL be a length-2 integer vector (the §2 primer encoding, with `NA_integer_` permitted as the reserved INT_MIN value), and `.local_envir` SHALL be an environment. Invalid input SHALL abort with an informative `chk`-style message.

#### Scenario: Non-whole-number seed
- **WHEN** `local_dqrng_state()` is called with a non-whole-number `seed`
- **THEN** the call SHALL abort with an informative error

#### Scenario: Malformed primer
- **WHEN** `local_dqrng_state()` is called with a `state` that is not a length-2 integer vector
- **THEN** the call SHALL abort with an informative error

#### Scenario: NA_integer_ accepted in the primer
- **WHEN** `local_dqrng_state()` is called with a `state` whose elements include `NA_integer_` (the INT_MIN encoding of §2)
- **THEN** the call SHALL be accepted and seed dqrng without error
