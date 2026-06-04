## MODIFIED Requirements

### Requirement: Reproducible fitting
Given the same input tibble and the same `seed`, the function SHALL produce identical `fits` output across invocations, seeding each per-task fit via the dqrng + per-task primer contract — `local_dqrng_state(seed, task_primer(identity))` under a scenario-scoped, reentrant `local_dqrng_backend()` — rather than the L'Ecuyer-CMRG sub-stream lattice. The per-task primer SHALL be derived from the fit task's identity (its `sim`/`stream` together with its fit-grid row: `nrow`, `rescale`, `computable`, `at_boundary_ok`, the `min_pmix` name, `range_shape1`, `range_shape2`), so results are independent of the order in which fits run. The public `min_pmix` argument SHALL remain a list of functions; its identity enters the primer **by name**, not by function value, so a recompile or JIT does not move a task's primer.

#### Scenario: Deterministic fits
- **WHEN** `ssd_fit_dists_sims()` is invoked twice with the same `x` and same `seed`
- **THEN** the resulting `fits` columns SHALL be identical

#### Scenario: Order-independent fits
- **WHEN** a fit task is computed standalone and again as one row of a larger fit grid, with the same `seed` and task identity
- **THEN** the two `fitdists` results SHALL be identical
