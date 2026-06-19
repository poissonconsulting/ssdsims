## REMOVED Requirements

### Requirement: Unified scenario dispatch
**Reason**: The monolith `ssd_run_scenario()` in-memory runner is retired by the targets redesign; once `migrate-public-api` makes the declarative + sharded surface canonical, the generic and its five S3 methods are unreachable and are deleted with the rest of the monolith path.
**Migration**: Define a scenario with `ssd_define_scenario()` (or `ssd_scenario_data()` / `ssd_gen()` for the generator inputs this dispatch previously accepted) and run it with `ssd_run_scenario_baseline()` (in-memory) or `ssd_run_scenario_shards()` (Hive-Parquet sharded).

### Requirement: End-to-end pipeline semantics
**Reason**: Removed with the monolith runner — the three-stage in-memory chain (`ssd_sim_data()` → `ssd_fit_dists_sims()` → `ssd_hc_sims()`) it described no longer exists.
**Migration**: The end-to-end pipeline is now the sample/fit/hc step runners driven by `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` over the per-step task tables.

### Requirement: Unified argument surface
**Reason**: Removed with the monolith runner — there is no longer a scenario-level generic that forwards arguments to the underlying simulate/fit/HC steps.
**Migration**: Configure simulate/fit/HC behaviour through `ssd_define_scenario()` and the scenario settings consumed by the step runners.

### Requirement: Progress reporting
**Reason**: Removed with the monolith runner — the `.progress` surface belonged to the in-memory `ssd_run_scenario()` loop.
**Migration**: Progress for the sharded path is reported by the `targets`/`crew` machinery driving `ssd_run_scenario_shards()`.
