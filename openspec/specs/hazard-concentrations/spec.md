# hazard-concentrations Specification

## Purpose

Estimate hazard concentrations (HCs) — with or without bootstrap confidence intervals — for each fitted distribution object in a simulation tibble, expanding across the full factorial of estimation and CI methods, so simulation studies can compare HC estimators across scenarios.

## Requirements

### Requirement: HC estimation for fitted simulations
The package SHALL expose `ssd_hc_sims(x, ...)` that accepts a tibble with integer `sim` and `stream` columns and a list column `fits`, and returns the tibble augmented with a list column `hc` holding per-row HC tibbles.

#### Scenario: Basic HC computation
- **WHEN** `ssd_hc_sims()` is called with a tibble produced by `ssd_fit_dists_sims()`
- **THEN** the returned tibble SHALL include all original columns plus an `hc` list column whose entries are tibbles produced by `ssdtools::ssd_hc()`

#### Scenario: Empty input
- **WHEN** `ssd_hc_sims()` is called with a tibble of zero rows
- **THEN** the function SHALL return the input with an empty `hc` list column and without raising an error

### Requirement: Factorial expansion over HC configurations
The function SHALL cross-join the input tibble with the cartesian product of the vector-valued bootstrap axes `nboot`, `ci_method`, and `parametric`, producing one row per (simulation × bootstrap-configuration) combination. `est_method` SHALL NOT be part of this factorial expansion: it is an hc scenario setting whose every requested value is summarised from each cell's single bootstrap sample set (see "est_method summaries derived from a single bootstrap sample set").

#### Scenario: Multiple CI methods
- **WHEN** `ssd_hc_sims()` is called with `ci_method = c("multi_fixed", "weighted_samples")` on an input of `N` rows
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, ci_method) pair

#### Scenario: Multiple est_methods do not multiply rows
- **WHEN** `ssd_hc_sims()` is called with `est_method = c("arithmetic", "multi")` (and otherwise scalar bootstrap axes) on an input of `N` rows
- **THEN** the returned tibble SHALL still have `N` rows (one per simulation × bootstrap-configuration), each carrying an `hc` tibble whose rows cover both `est_method` values

### Requirement: est_method summaries derived from a single bootstrap sample set
For each (simulation × hc-configuration) cell, `ssd_hc_sims()` SHALL run the
bootstrap **once** and derive the summary for **every** requested `est_method`
from that single retained sample set, rather than re-bootstrapping per method.
The `hc` tibble for a cell SHALL contain one row per requested `est_method`.
**At a fixed cell seed**, each row's `est`/`se`/`lcl`/`ucl` SHALL be
byte-identical to calling `ssdtools::ssd_hc()` for that `est_method` alone with
that **same seed** and inputs (the point `est` is analytical and
seed-independent; the CI is est_method-invariant given a shared draw — see
`exploration/est-method-invariance.R`). This is a same-seed invariant, NOT a
claim of equality with the pre-change pipeline: because the hc primer hashes the
hc-grid row including `est_method` today, removing the axis re-seeds each hc task,
so collapsed-pipeline CIs differ numerically from the old per-`est_method`-seeded
output (point estimates are unchanged). `est_method` SHALL NOT appear in the
function's factorial expansion; it is a within-cell dimension.

#### Scenario: One bootstrap, all est_methods, same-seed identity
- **WHEN** `ssd_hc_sims()` computes one hc cell at a fixed seed with `ci = TRUE` and `est_method = c("arithmetic", "geometric", "multi")`
- **THEN** the cell's `hc` tibble SHALL contain one row per `est_method`, all derived from one shared bootstrap sample set, and each row SHALL equal the single-method `ssdtools::ssd_hc()` result seeded with that **same** cell seed (no per-method re-bootstrap)

#### Scenario: est_method does not multiply bootstrap work
- **WHEN** `ssd_hc_sims()` is called on `N` input rows with `ci = TRUE` and `M` `est_method` values
- **THEN** the number of bootstrap passes SHALL be independent of `M` (one per `nboot × ci_method × parametric` cell, not `M` times that), while the returned `hc` rows still cover all `M` methods

### Requirement: Default HC configuration
Default argument values SHALL be `proportion = 0.05`, `ci = FALSE`, `nboot = 1000`, `est_method = "multi"`, `ci_method = "weighted_samples"`, and `parametric = TRUE`.

#### Scenario: Omitted arguments use defaults
- **WHEN** `ssd_hc_sims()` is called with only `x`
- **THEN** the function SHALL compute a point estimate (no CI) for the 5th-percentile hazard concentration using `est_method = "multi"`

### Requirement: Argument validation
The function SHALL validate arguments against the methods exposed by `ssdtools` and abort on invalid input. `ci` SHALL be validated as a flag (a single non-`NA` `TRUE`/`FALSE`); it is applied as a scalar and is not part of the function's factorial expansion.

#### Scenario: Unknown est_method
- **WHEN** `est_method` contains a value not in `ssdtools::ssd_est_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Unknown ci_method
- **WHEN** `ci_method` contains a value not in `ssdtools::ssd_ci_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Missing fits column
- **WHEN** the input tibble lacks a `fits` column
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: ci must be a flag
- **WHEN** `ssd_hc_sims()` is called with a non-flag `ci` (e.g. `ci = c(FALSE, TRUE)`)
- **THEN** `ssd_hc_sims()` SHALL abort with an error, since `ci` is a scalar flag

### Requirement: min_pboot is not user-configurable
The function SHALL reserve `min_pboot = 0` and reject any user attempt to override it.

#### Scenario: User passes min_pboot
- **WHEN** `ssd_hc_sims()` is called with `min_pboot` in `...`
- **THEN** the function SHALL abort with an error stating that `min_pboot` is fixed at 0 and cannot be set by the user

### Requirement: Reproducible bootstrapping
Given the same inputs and the same `seed`, bootstrap-based HC estimates SHALL be identical across invocations.

#### Scenario: Deterministic bootstrap
- **WHEN** `ssd_hc_sims()` is invoked twice with the same `x`, `ci = TRUE`, and same `seed`
- **THEN** the resulting `hc` tibbles SHALL be identical

### Requirement: Optional on-disk persistence of bootstrap samples
The function SHALL accept a `save_to` directory path and, when supplied, persist bootstrap outputs to that directory.

#### Scenario: save_to must be a directory
- **WHEN** `ssd_hc_sims()` is called with a non-null `save_to` that is not an existing directory
- **THEN** the function SHALL abort with an error

#### Scenario: save_to null by default
- **WHEN** `ssd_hc_sims()` is called without `save_to`
- **THEN** the function SHALL compute HCs entirely in memory without writing files
