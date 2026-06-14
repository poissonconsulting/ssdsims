## ADDED Requirements

### Requirement: dists declares one or more distribution sets
`ssd_define_scenario()` SHALL accept `dists` as either a bare character vector
(one anonymous **distribution set** — today's behaviour, no new axis) or a
**named list of character vectors**. A distribution set is the pool of
distributions model-averaged together to form one SSD (one `est`); each named
list entry is one such set keyed by a
unique, non-missing, filesystem-safe name (the name becomes a `distset=` Hive
path segment). Each set's members SHALL be a non-empty character vector drawn
from `ssdtools::ssd_dists_all()`, unique and non-missing. The constructor SHALL
derive the fit **union** `sort(unique(unlist(dists)))` and store it as
`scenario$fit$dists` (the single model-averaged superset fit every set is a subset
of), and SHALL store the named sets as `scenario$hc$distsets` for the hc step to
subset by name. The set **names** — not their members — are what hash onto the
hc task path (mirroring `min_pmix` and dataset name referencing); the member
vectors are carried for execution and SHALL NOT enter any task hash.

#### Scenario: Bare character vector is one anonymous set
- **WHEN** `ssd_define_scenario(..., dists = ssdtools::ssd_dists_bcanz())` is called
- **THEN** `scenario$fit$dists` SHALL be that vector, `scenario$hc$distsets` SHALL be a single set, and no `distset` cross-join axis SHALL be introduced (one hc pool, today's behaviour)

#### Scenario: Named list declares multiple pools and a fit union
- **WHEN** `ssd_define_scenario(..., dists = list(BCANZ = ssdtools::ssd_dists_bcanz(), Iwasaki = c("burrIII3", "gamma", "llogis", "lnorm", "weibull"), lnorm = "lnorm"))` is called
- **THEN** `scenario$fit$dists` SHALL be the sorted union of all three sets' members, and `scenario$hc$distsets` SHALL be the named list of the three sets verbatim

#### Scenario: Set members carried for execution but not hashed
- **WHEN** a scenario is constructed with a named-list `dists`
- **THEN** the object SHALL carry each set's member vector keyed by set name for execution, and those member vectors SHALL NOT contribute to any task hash (the hc task path carries the set name only)

#### Scenario: Invalid set membership rejected
- **WHEN** `dists` is a named list with a set whose member is not in `ssdtools::ssd_dists_all()`, or with a duplicate/missing set name, or an empty set
- **THEN** the constructor SHALL abort, in the context of the user-facing function, with an informative error naming the offending set

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
- **WHEN** a scenario is constructed with a named-list `dists`
- **THEN** `scenario$fit$dists` SHALL be the sorted union of every set's members, and `scenario$hc$distsets` SHALL carry the named sets (their member vectors) for the hc step to subset by name
