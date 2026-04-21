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
The function SHALL cross-join the input tibble with the cartesian product of vector-valued arguments `nboot`, `est_method`, `ci_method`, and `parametric`, producing one row per (simulation × HC-configuration) combination.

#### Scenario: Multiple CI methods
- **WHEN** `ssd_hc_sims()` is called with `ci_method = c("multi_fixed", "weighted_samples")` on an input of `N` rows
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, ci_method) pair

### Requirement: Default HC configuration
Default argument values SHALL be `proportion = 0.05`, `ci = FALSE`, `nboot = 1000`, `est_method = "multi"`, `ci_method = "weighted_samples"`, and `parametric = TRUE`.

#### Scenario: Omitted arguments use defaults
- **WHEN** `ssd_hc_sims()` is called with only `x`
- **THEN** the function SHALL compute a point estimate (no CI) for the 5th-percentile hazard concentration using `est_method = "multi"`

### Requirement: Argument validation
The function SHALL validate arguments against the methods exposed by `ssdtools` and abort on invalid input.

#### Scenario: Unknown est_method
- **WHEN** `est_method` contains a value not in `ssdtools::ssd_est_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Unknown ci_method
- **WHEN** `ci_method` contains a value not in `ssdtools::ssd_ci_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Missing fits column
- **WHEN** the input tibble lacks a `fits` column
- **THEN** `ssd_hc_sims()` SHALL abort with an error

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
