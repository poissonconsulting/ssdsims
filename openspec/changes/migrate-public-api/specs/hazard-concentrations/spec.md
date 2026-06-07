## MODIFIED Requirements

### Requirement: Reproducible bootstrapping
Given the same inputs and the same `seed`, bootstrap-based HC estimates SHALL be identical across invocations, seeding each per-task HC computation via the dqrng + per-task primer contract — `local_dqrng_state(seed, task_primer(identity))` under a scenario-scoped, reentrant `local_dqrng_backend()` — rather than the L'Ecuyer-CMRG sub-stream lattice. The per-task primer SHALL be derived from the HC task's identity (its `sim`/`stream` together with its hc-grid row: `nboot`, `est_method`, `ci_method`, `parametric`), so results are independent of the order in which HC tasks run. The scenario-wide scalar `ci` flag is applied uniformly to every HC task and SHALL NOT be part of the per-task identity — the point estimate is invariant to `ci`, so a constant `ci` term carries no task-distinguishing information (consistent with the `scalar-ci-flag` change, which removed `ci` from `task_axes("hc")`).

#### Scenario: Deterministic bootstrap
- **WHEN** `ssd_hc_sims()` is invoked twice with the same `x`, `ci = TRUE`, and same `seed`
- **THEN** the resulting `hc` tibbles SHALL be identical

#### Scenario: Order-independent bootstrap
- **WHEN** an HC task is computed standalone and again as one row of a larger HC grid, with the same `seed` and task identity
- **THEN** the two `hc` tibbles SHALL be identical
