## REMOVED Requirements

### Requirement: Fit distributions to nested simulations
**Reason**: The monolith `ssd_fit_dists_sims()` is retired with the in-memory runner; the targets redesign fits distributions through the declarative `fit` step (`fit_data_task()` / `fit_data_task_primer()`) over the per-step task tables.
**Migration**: Run the `fit` step via `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` on a scenario defined with `ssd_define_scenario()`.

### Requirement: Factorial expansion over fit configurations
**Reason**: Removed with `ssd_fit_dists_sims()` — the cross-join over `rescale`/`computable`/`at_boundary_ok`/`min_pmix`/`range_shape1`/`range_shape2` is now the fit task-table expansion.
**Migration**: The fit grid is enumerated into the fit task table by the scenario's fit settings.

### Requirement: Default fit configuration matches ssdtools
**Reason**: Removed with `ssd_fit_dists_sims()` — its defaults belonged to the monolith signature; the declarative `fit` step resolves `min_pmix` by name and carries the other defaults on the scenario.
**Migration**: Fit defaults are set on the scenario via `ssd_define_scenario()`.

### Requirement: Argument validation
**Reason**: Removed with `ssd_fit_dists_sims()`; fit-argument validation now happens at scenario-definition time.
**Migration**: Invalid fit configuration is rejected by `ssd_define_scenario()`.

### Requirement: Reproducible fitting
**Reason**: Removed with `ssd_fit_dists_sims()` — its L'Ecuyer-CMRG per-fit seeding is deleted. The `fit` step seeds per task via the dqrng `(seed, primer)` contract.
**Migration**: Reproducibility is provided by `local_dqrng_state(seed, task_primer(identity))` in the `fit` step (see the `parallel-safe-seeding` dqrng requirements).

### Requirement: Silent fitting by default
**Reason**: Removed with `ssd_fit_dists_sims()` — the silencing behaviour belonged to the monolith wrapper.
**Migration**: Fit-time messaging is governed by the step runner / `duckplyr-config` scope.
