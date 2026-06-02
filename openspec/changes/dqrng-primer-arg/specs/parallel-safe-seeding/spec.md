## MODIFIED Requirements

### Requirement: Scoped dqrng state installation from a (seed, primer) pair
The package SHALL provide `local_dqrng_state(seed, primer, .local_envir = parent.frame())`, a thin wrapper around `dqrng::dqset.seed(seed, stream = primer)` that installs the `(seed, primer)` starting point as the running RNG state and registers a deferred restore of the prior RNG state when `.local_envir` exits. The `primer` argument is the per-task primer (the value passed to dqrng's `stream` argument, per `TARGETS-DESIGN.md` §2 and GLOSSARY); the helper's `_state` name reflects that it scopes the running RNG *state*, while the value it installs is a *primer*. The package MAY additionally provide a `with_dqrng_state(seed, primer, code)` companion that delegates to `local_dqrng_state()` and evaluates `code`.

#### Scenario: Install and restore dqrng state
- **WHEN** `local_dqrng_state(seed, primer)` is called within a function and that function returns
- **THEN** the RNG state SHALL be set to `(seed, primer)` for the duration of the calling frame and SHALL be restored to its prior value when the frame exits

#### Scenario: Same seed and primer reproduce draws
- **WHEN** `local_dqrng_state(seed, primer)` is called twice with the same `(seed, primer)` and the same draws are taken under the active dqrng backend
- **THEN** the two draw sequences SHALL be identical

#### Scenario: Surrounding RNG stream undisturbed
- **WHEN** code takes RNG draws, calls `local_dqrng_state(seed, primer)` in a nested scope that itself takes draws, and then continues
- **THEN** after the nested scope exits the outer RNG stream SHALL continue as if the nested `local_dqrng_state()` scope had not consumed any RNG

### Requirement: Abort when the dqrng backend is not active
`local_dqrng_state()` (and `with_dqrng_state()` if provided) SHALL verify that the dqrng backend is active before seeding, using the `dqrng_backend_active()` helper provided by `dqrng-init`. If the backend is not active, the function SHALL abort with an informative error directing the caller to open a `local_dqrng_backend()` scope first, rather than seeding the wrong generator.

#### Scenario: Abort outside an active backend scope
- **WHEN** `local_dqrng_state(seed, primer)` is called while the dqrng backend is not active (`dqrng_backend_active()` is `FALSE`)
- **THEN** the call SHALL abort with an informative error instructing the caller to activate the backend via `local_dqrng_backend()`

#### Scenario: Proceed inside an active backend scope
- **WHEN** `local_dqrng_state(seed, primer)` is called within an active `local_dqrng_backend()` scope
- **THEN** the RNG-kind check SHALL pass and the function SHALL install the `(seed, primer)` starting point

### Requirement: dqrng state argument validation
`local_dqrng_state()` (and `with_dqrng_state()` if provided) SHALL validate its arguments: `seed` SHALL be a whole number, `primer` SHALL be a length-2 integer vector (the §2 primer encoding, with `NA_integer_` permitted as the reserved INT_MIN value), and `.local_envir` SHALL be an environment. Invalid input SHALL abort with an informative `chk`-style message.

#### Scenario: Non-whole-number seed
- **WHEN** `local_dqrng_state()` is called with a non-whole-number `seed`
- **THEN** the call SHALL abort with an informative error

#### Scenario: Malformed primer
- **WHEN** `local_dqrng_state()` is called with a `primer` that is not a length-2 integer vector
- **THEN** the call SHALL abort with an informative error

#### Scenario: NA_integer_ accepted in the primer
- **WHEN** `local_dqrng_state()` is called with a `primer` whose elements include `NA_integer_` (the INT_MIN encoding of §2)
- **THEN** the call SHALL be accepted and seed dqrng without error
