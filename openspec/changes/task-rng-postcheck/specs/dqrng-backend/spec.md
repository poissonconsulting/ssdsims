## ADDED Requirements

### Requirement: dqrng-specific backend integrity witness
The package SHALL provide an internal guard that verifies **dqrng specifically** — not merely *some* user-supplied RNG — currently holds base R's user-supplied RNG slot, and aborts otherwise. Because base R has a single process-global user-supplied RNG resolved by the `user_unif_rand` symbol across all loaded DLLs, a foreign user-RNG package can hold the slot while `RNGkind()[1]` still reports `"user-supplied"`; the existing `RNGkind()`-based `dqrng_backend_active()` probe SHALL NOT be relied on to distinguish dqrng from such a foreign generator. The witness SHALL use dqrng's own state as evidence: record `dqrng::dqrng_get_state()`, take one base-R draw (which routes through the `user_unif_rand` slot), and re-read the state — the backend is intact if and only if dqrng's state advanced.

#### Scenario: Intact when dqrng holds the slot
- **WHEN** the dqrng backend is active and dqrng is the generator bound to base R's `user_unif_rand` slot, and the integrity witness is run
- **THEN** the recorded dqrng state SHALL have advanced after the base-R draw, and the guard SHALL return invisibly (the backend is intact)

#### Scenario: Aborts when a foreign user-RNG holds the slot
- **WHEN** a foreign user-supplied RNG (e.g. a second package such as `randtoolbox` loaded later) is bound to the `user_unif_rand` slot — so `RNGkind()[1]` still reports `"user-supplied"` — and the integrity witness is run
- **THEN** dqrng's recorded state SHALL be unchanged after the base-R draw (the draw advanced the foreign generator instead), and the guard SHALL abort with an informative error reported in the user-facing frame

#### Scenario: Witness is non-destructive
- **WHEN** the integrity witness runs (recording dqrng's state, drawing once, then restoring the recorded state via `dqrng::dqrng_set_state()`)
- **THEN** the dqrng draw sequence after the witness SHALL be identical to the sequence that would have been produced had the witness not run — the check SHALL consume no net randomness
