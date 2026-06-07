## MODIFIED Requirements

### Requirement: Declarative-only field set
The `ssdsims_scenario` object SHALL store `seed` (a scalar integer), `nsim`, `nrow`, dataset names, the `fit` and `hc` argument-vector grids, and `partition_by`, and SHALL NOT store materialized task tables or RNG states. It SHALL NOT carry an `upload` field: the upload destination is an execution concern supplied to the runner (`ssd_scenario_targets(..., upload = ...)`), not part of the scenario's declarative identity. For name-referenced parameters it additionally carries the **materialised value needed for execution** — the `min_pmix` functions, keyed by name — which are used only when running a task and SHALL NOT enter any task hash (task identities use the names, not the values).

#### Scenario: Seed is a scalar integer
- **WHEN** a scenario is constructed with `seed = 42L`
- **THEN** the object SHALL store `seed` as a single integer that fully re-roots the scenario's RNG when changed

#### Scenario: partition_by defaults are populated
- **WHEN** `ssd_define_scenario()` is called without an explicit `partition_by`
- **THEN** the object SHALL carry the documented per-step defaults (data, fit, hc path axes)

#### Scenario: No upload field on the scenario
- **WHEN** an `ssdsims_scenario` is constructed
- **THEN** the object SHALL NOT contain an `upload` field, and `ssd_define_scenario()` SHALL NOT accept an `upload` argument

#### Scenario: min_pmix functions are carried for execution but not hashed
- **WHEN** a scenario is constructed with a `min_pmix` reference
- **THEN** the object SHALL carry the resolved `min_pmix` function keyed by name for execution, and that function value SHALL NOT contribute to any task hash
