## MODIFIED Requirements

### Requirement: Non-overlapping sub-streams for simulations
The package SHALL continue to provide L'Ecuyer-CMRG per-simulation sub-stream generation — advancing sub-streams so that `nsim` simulations within a single stream use statistically independent seeds — as a **deprecated one-release shim**, retained only to back the un-migrated generator methods (`ssd_sim_data.function`/`.character`/`.fitdists`/`.tmbfit`) until `cleanup-lecuyer`. The migrated public step functions (`ssd_sim_data.data.frame()`, `ssd_fit_dists_sims()`, `ssd_hc_sims()`) SHALL instead obtain per-simulation independence from distinct per-task dqrng primers (`task_primer()`), not L'Ecuyer sub-streams.

#### Scenario: Legacy helper still advances sub-streams
- **WHEN** per-simulation seeds are generated via the surviving L'Ecuyer helper for `nsim > 1` within a fixed `stream` and `start_sim`
- **THEN** each simulation SHALL receive a distinct L'Ecuyer-CMRG sub-stream seed advanced via `parallel::nextRNGSubStream()`

#### Scenario: Migrated functions use per-task primers
- **WHEN** the migrated `ssd_sim_data.data.frame()` produces `nsim > 1` simulations for a fixed `seed` and `stream`
- **THEN** each simulation's draw SHALL be seeded by its own dqrng primer (`task_primer()` over the task identity), independent of `parallel::nextRNGSubStream()`

### Requirement: Stream isolation
Distinct `stream` values SHALL yield independent RNG sequences. For the surviving L'Ecuyer-CMRG shim this is achieved via non-overlapping top-level streams (`parallel::nextRNGStream()`); for the migrated public step functions `stream` is a component of the per-task primer identity, so distinct `stream` values produce distinct primers and therefore independent dqrng draws.

#### Scenario: Legacy streams remain non-overlapping
- **WHEN** seeds are generated via the L'Ecuyer helper for the same `seed` and `start_sim` but different `stream` values
- **THEN** the resulting seeds SHALL correspond to different top-level L'Ecuyer-CMRG streams

#### Scenario: Migrated functions isolate by primer
- **WHEN** a migrated public step function is called with the same `seed` but different `stream` values
- **THEN** the per-task primers SHALL differ, yielding independent dqrng draws

### Requirement: Seed independence from caller RNG state
The package's seeding paths SHALL NOT leave the caller's global RNG state modified on return, regardless of whether a `seed` argument is supplied. For the migrated public step functions this is provided by the scenario-scoped `local_dqrng_backend()` + `local_dqrng_state()`, which restore the prior RNG kind and state on scope exit; the surviving L'Ecuyer shim restores `.Random.seed`/`RNGkind()` as before.

#### Scenario: Global RNG restored
- **WHEN** a migrated public step function (`ssd_sim_data.data.frame()`, `ssd_fit_dists_sims()`, `ssd_hc_sims()`) runs to completion
- **THEN** `.Random.seed` and `RNGkind()` in the caller's global environment SHALL be identical before and after the call

#### Scenario: NULL seed still reproducible per-stream
- **WHEN** the L'Ecuyer shim is invoked with `seed = NULL`
- **THEN** the helper SHALL use the current global RNG state to seed L'Ecuyer-CMRG without persistently modifying it
