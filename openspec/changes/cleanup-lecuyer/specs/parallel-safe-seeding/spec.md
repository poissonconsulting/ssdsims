## REMOVED Requirements

### Requirement: Scoped L'Ecuyer-CMRG seeding from a scalar seed
**Reason**: The L'Ecuyer-CMRG RNG backend is removed; `local_lecuyer_cmrg_seed()` / `with_lecuyer_cmrg_seed()` and the file `R/lecuyer-cmrg-seed.R` are deleted. The package keeps a single RNG path (dqrng).
**Migration**: Seed reproducibly with `local_dqrng_state(seed, primer)` / `with_dqrng_state()` under a `local_dqrng_backend()` scope.

### Requirement: Scoped L'Ecuyer-CMRG state installation from a state vector
**Reason**: Removed with the L'Ecuyer-CMRG backend — `local_lecuyer_cmrg_state()` / `with_lecuyer_cmrg_state()` and the length-7 `.Random.seed` state machinery are deleted.
**Migration**: Install per-task state with `local_dqrng_state(seed, primer)`, where the length-2 integer `primer` is derived via `task_primer()`.

### Requirement: Seed and state argument validation
**Reason**: Removed with the L'Ecuyer-CMRG scoped helpers whose `seed`/`state` arguments this validated.
**Migration**: The dqrng state helpers carry their own argument validation (see *dqrng state argument validation*).

### Requirement: Non-overlapping sub-streams for simulations
**Reason**: Removed with the L'Ecuyer-CMRG backend — the `parallel::nextRNGSubStream()` per-simulation sub-stream lattice is deleted. Per-simulation independence now comes from distinct per-task dqrng primers.
**Migration**: Distinct tasks receive distinct `task_primer()` identities, yielding independent dqrng draws (see *Per-task seed-and-run wrappers install a primer exactly once*).

### Requirement: Stream isolation
**Reason**: Removed with the L'Ecuyer-CMRG backend — `parallel::nextRNGStream()` top-level stream isolation is deleted.
**Migration**: A `stream`/`dataset` coordinate is a component of the per-task primer identity, so distinct streams produce independent, order-independent draws.

### Requirement: Seed independence from caller RNG state
**Reason**: Removed with the L'Ecuyer-CMRG seed-generating helpers and `ssd_sim_data()` that it scoped.
**Migration**: `local_dqrng_backend()` and `local_dqrng_state()` restore the caller's prior RNG kind and state on scope exit (see *Per-task dqrng backend integrity brackets*), preserving caller-state independence on the dqrng path.
