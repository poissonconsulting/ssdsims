## MODIFIED Requirements

### Requirement: Declarative-only field set
The `ssdsims_scenario` object SHALL store `seed` (a scalar integer), `nsim`, `nrow`, dataset names, the `fit` and `hc` argument-vector grids, `partition_by`, and an optional `upload` spec, and SHALL NOT store materialized task tables or RNG states. For name-referenced parameters it additionally carries the **materialised value needed for execution** — the `min_pmix` functions, keyed by name — which are used only when running a task and SHALL NOT enter any task hash (task identities use the names, not the values).

#### Scenario: Seed is a scalar integer
- **WHEN** a scenario is constructed with `seed = 42L`
- **THEN** the object SHALL store `seed` as a single integer that fully re-roots the scenario's RNG when changed

#### Scenario: partition_by defaults are populated
- **WHEN** `ssd_define_scenario()` is called without an explicit `partition_by`
- **THEN** the object SHALL carry the documented per-step defaults (data, fit, hc path axes)

#### Scenario: Upload defaults to none
- **WHEN** `ssd_define_scenario()` is called without an `upload` argument
- **THEN** the object's `upload` field SHALL be `NULL`

#### Scenario: min_pmix functions are carried for execution but not hashed
- **WHEN** a scenario is constructed with a `min_pmix` reference
- **THEN** the object SHALL carry the resolved `min_pmix` function keyed by name for execution, and that function value SHALL NOT contribute to any task hash

### Requirement: min_pmix referenced by name
`ssd_define_scenario()` SHALL store `min_pmix` in the `fit` grid as one or more names (a character vector) — used for hashing and the task path — and SHALL additionally materialise the resolved single-argument **functions**, keyed by name, on the scenario for execution. A supplied function SHALL be stored under its derived name; a name-string reference SHALL be resolved to a function at construction (from `ssdtools` or the caller's environment), failing fast if it cannot be resolved to a single-argument function. The stored names — and therefore task hashes — SHALL be unchanged by the materialisation, and the function values SHALL NOT be hashed. Resolution of a name back to a function is the scenario accessor `scenario_min_pmix()` (this change), not a separate registry.

#### Scenario: min_pmix function is stored by name and materialised
- **WHEN** `ssd_define_scenario(..., min_pmix = ssdtools::ssd_min_pmix)` (or the default) is called
- **THEN** the object's `fit$min_pmix` SHALL be the derived name (e.g. `"ssd_min_pmix"`) as a character vector, and the resolved function SHALL be materialised on the scenario keyed by that name

#### Scenario: min_pmix names accepted directly and resolved
- **WHEN** `ssd_define_scenario(..., min_pmix = c("default", "strict"))` is called
- **THEN** the object SHALL store those names verbatim as `fit$min_pmix` and SHALL materialise the function each name resolves to, keyed by name

#### Scenario: min_pmix list of functions derives names
- **WHEN** `ssd_define_scenario(..., min_pmix = list(ssdtools::ssd_min_pmix))` (unnamed) or `list(strict = ssdtools::ssd_min_pmix)` (named) is called
- **THEN** the object SHALL store the derived element name(s) (e.g. `"ssd_min_pmix"`) or the list names (e.g. `"strict"`) respectively, validate each provided function before taking its name, and materialise the functions keyed by those names
