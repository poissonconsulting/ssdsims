## REMOVED Requirements

### Requirement: Dispatch by input type
**Reason**: The monolith `ssd_sim_data()` generic and its five S3 methods are retired with the in-memory runner; the targets redesign generates simulated data through the declarative `sample` step (`sample_data_task()` / `sample_data_task_primer()`) instead.
**Migration**: Supply the input via `ssd_scenario_data()` (data frames) or `ssd_gen()` (functions / named generators / `fitdists` / `tmbfit`) to `ssd_define_scenario()`, then run the `sample` step through `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`.

### Requirement: Nested tibble output
**Reason**: Removed with `ssd_sim_data()` — the `(sim, stream, nrow, replace, data)` nested tibble was the monolith method's return shape.
**Migration**: The sample step emits its rows through the task-table / shard schema consumed by the fit step.

### Requirement: Input validation
**Reason**: Removed with `ssd_sim_data()`; input validation now happens in `ssd_define_scenario()` / `ssd_scenario_data()` / `ssd_gen()`.
**Migration**: Errors for malformed simulate inputs are raised at scenario-definition time.

### Requirement: Reproducible stream-aware seeding
**Reason**: Removed with `ssd_sim_data()` — its L'Ecuyer-CMRG stream seeding is deleted. The sample step seeds per task via the dqrng `(seed, primer)` contract.
**Migration**: Reproducibility is provided by `local_dqrng_state(seed, task_primer(identity))` under `local_dqrng_backend()` in the `sample` step (see the `parallel-safe-seeding` dqrng requirements).

### Requirement: Global stream default
**Reason**: Removed with `ssd_sim_data()` — the `stream` argument default belonged to the monolith method signature.
**Migration**: The per-task `stream`/`dataset` coordinate is carried by the task tables, not a function argument default.
