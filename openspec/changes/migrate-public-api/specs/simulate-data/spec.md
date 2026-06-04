## MODIFIED Requirements

### Requirement: Reproducible stream-aware seeding
The `data.frame` method SHALL produce identical output for identical combinations of `seed`, `stream`, and `start_sim`, seeding each per-simulation draw via the dqrng + per-task primer contract — `local_dqrng_state(seed, task_primer(identity))` under a scenario-scoped, reentrant `local_dqrng_backend()` — rather than the L'Ecuyer-CMRG sub-stream lattice. The `stream` and `sim` values SHALL be components of the per-task primer identity, so distinct `stream` (or `sim`) values yield independent draws and results are independent of the order in which simulations run. The shared `n_max = max(nrow)` draw SHALL be taken once per `(stream, sim, replace)` primer and truncated per `nrow` with `head()` (RNG-free), so all `nrow` values share one seeded draw. The generator methods (`function`/`character`/`fitdists`/`tmbfit`) retain the L'Ecuyer-CMRG path as a deprecated shim until `cleanup-lecuyer`.

#### Scenario: Same seed, same stream
- **WHEN** the `data.frame` method is called twice with the same `seed`, `stream`, and `start_sim`
- **THEN** both calls SHALL return identical simulated datasets

#### Scenario: Different streams
- **WHEN** the `data.frame` method is called with the same `seed` but different `stream` values
- **THEN** the calls SHALL return different simulated datasets, because `stream` is part of each task's primer identity

#### Scenario: Shared draw truncated per nrow
- **WHEN** the `data.frame` method is called with a vector `nrow` for a fixed `(seed, stream, sim, replace)`
- **THEN** each `nrow` dataset SHALL be the `head(., nrow)` prefix of the shared `n_max = max(nrow)` draw

#### Scenario: Order-independent results
- **WHEN** the same `(seed, stream, sim, nrow, replace)` simulation is produced standalone and as part of a larger `nsim`/`nrow` grid
- **THEN** its simulated dataset SHALL be identical, because its primer fully determines its RNG starting point
