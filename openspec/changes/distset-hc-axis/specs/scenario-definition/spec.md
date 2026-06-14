## ADDED Requirements

### Requirement: ssd_distset() declares one or more distribution sets
The package SHALL expose `ssd_distset(...)` as the single entry point that
assembles one or more distribution sets into a validated, named collection — an
`ssdsims_distset` object (a named list of distribution-name character vectors). A
distribution set is the pool of distributions model-averaged together to form one
SSD (one `est`). Set names SHALL be taken from the `...` argument names, unique,
non-missing, and filesystem-safe (each becomes a `distset=` Hive path segment).
Each set SHALL be a non-empty, unique, non-`NA` character vector whose members are
a subset of `ssdtools::ssd_dists_all()`. `ssd_define_scenario(dists = ...)` SHALL
accept **only** an `ssd_distset()` collection; it SHALL derive the fit **union**
`sort(unique(unlist(dists)))` and store it as `scenario$fit$dists` (the single
model-averaged superset fit every set is a subset of), and SHALL store the named
sets as `scenario$hc$distsets` for the hc step to subset by name. The set
**names** — not their members — are what hash onto the hc task path (mirroring
`min_pmix` and dataset name referencing); the member vectors are carried for
execution and SHALL NOT enter any task hash.

#### Scenario: Named sets assembled into a collection and a fit union
- **WHEN** `ssd_define_scenario(..., dists = ssd_distset(BCANZ = ssdtools::ssd_dists_bcanz(), Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"), lnorm = "lnorm"))` is called
- **THEN** `scenario$fit$dists` SHALL be the sorted union of all three sets' members, and `scenario$hc$distsets` SHALL be the named sets verbatim

#### Scenario: A bare dists input is rejected with a pointer to the constructor
- **WHEN** `ssd_define_scenario(..., dists = ssdtools::ssd_dists_bcanz())` (a bare character vector) or `dists = list(...)` (a plain list) is called
- **THEN** the constructor SHALL abort, in the user-facing function's context, with an informative error instructing the caller to supply an `ssd_distset()` collection

#### Scenario: Set members carried for execution but not hashed
- **WHEN** a scenario is constructed with an `ssd_distset()` collection
- **THEN** the object SHALL carry each set's member vector keyed by set name for execution, and those member vectors SHALL NOT contribute to any task hash (the hc task path carries the set name only)

#### Scenario: Invalid set membership rejected
- **WHEN** `ssd_distset()` is given a set whose member is not in `ssdtools::ssd_dists_all()`, an empty set, or a duplicate/missing set name
- **THEN** it SHALL abort with an informative error naming the offending set

## MODIFIED Requirements

### Requirement: Declarative-only field set
The `ssdsims_scenario` object SHALL store `seed` (a scalar integer), `nsim`, `nrow`, dataset names, the `fit` and `hc` argument-vector grids, and `partition_by`, and SHALL NOT store materialized task tables or RNG states. It SHALL NOT carry an `upload` field: the upload destination is an execution concern supplied to the runner (`ssd_scenario_targets(..., upload = ...)`), not part of the scenario's declarative identity. For name-referenced parameters it additionally carries the **materialised value needed for execution** — the `min_pmix` functions keyed by name, and the **distribution-set member vectors keyed by set name** — which are used only when running a task and SHALL NOT enter any task hash (task identities use the names, not the values). The `fit` grid's `dists` field SHALL be the **union** of all declared distribution sets (the single model-averaged superset fit), and the `hc` grid SHALL carry the named sets as `hc$distsets`.

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

#### Scenario: fit stores the union and hc stores the named sets
- **WHEN** a scenario is constructed with an `ssd_distset()` collection for `dists`
- **THEN** `scenario$fit$dists` SHALL be the sorted union of every set's members, and `scenario$hc$distsets` SHALL carry the named sets (their member vectors) for the hc step to subset by name
