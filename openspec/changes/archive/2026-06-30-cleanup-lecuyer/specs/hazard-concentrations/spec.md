## REMOVED Requirements

### Requirement: HC estimation for fitted simulations
**Reason**: The monolith `ssd_hc_sims()` is retired with the in-memory runner; the targets redesign estimates hazard concentrations through the declarative `hc` step (`hc_data_task()` / `hc_data_task_primer()`) over the per-step task tables.
**Migration**: Run the `hc` step via `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()` on a scenario defined with `ssd_define_scenario()`.

### Requirement: Factorial expansion over HC configurations
**Reason**: Removed with `ssd_hc_sims()` — the cross-join over `nboot`/`ci_method`/`parametric` is now the hc task-table expansion.
**Migration**: The hc grid is enumerated into the hc task table by the scenario's hc settings.

### Requirement: est_method summaries derived from a single bootstrap sample set
**Reason**: Removed with `ssd_hc_sims()` — the single-bootstrap, multi-`est_method` summary behaviour now lives in the declarative `hc` step (the `est-method-setting` contract).
**Migration**: The `hc` step summarises every requested `est_method` from one retained bootstrap sample set per cell.

### Requirement: Default HC configuration
**Reason**: Removed with `ssd_hc_sims()` — its defaults belonged to the monolith signature; hc defaults are now scenario settings.
**Migration**: HC defaults are set on the scenario via `ssd_define_scenario()`.

### Requirement: Argument validation
**Reason**: Removed with `ssd_hc_sims()`; hc-argument validation now happens at scenario-definition time.
**Migration**: Invalid hc configuration is rejected by `ssd_define_scenario()`.

### Requirement: min_pboot is not user-configurable
**Reason**: Removed with `ssd_hc_sims()` — the `min_pboot` guard belonged to the monolith wrapper.
**Migration**: The `hc` step retains the reserved-`min_pboot` behaviour internally.

### Requirement: Reproducible bootstrapping
**Reason**: Removed with `ssd_hc_sims()` — its L'Ecuyer-CMRG per-bootstrap seeding is deleted. The `hc` step seeds per task via the dqrng `(seed, primer)` contract.
**Migration**: Reproducibility is provided by `local_dqrng_state(seed, task_primer(identity))` in the `hc` step (see the `parallel-safe-seeding` dqrng requirements).

### Requirement: Optional on-disk persistence of bootstrap samples
**Reason**: Removed with `ssd_hc_sims()` — the `save_to` surface belonged to the monolith wrapper; the sharded path persists samples through the dual-summary outputs.
**Migration**: Bootstrap-sample retention is controlled by the scenario's `hc$samples` setting and the `dual-summary-outputs` path.

### Requirement: Per-cell distribution-set subsetting reuses one union fit
**Reason**: Removed with `ssd_hc_sims()` — the union-fit reuse behaviour now lives in the declarative `hc` step / the `hc-readout-aggregation` work.
**Migration**: Distribution-set subsetting reuse is handled by the `hc` step over the union fits.
