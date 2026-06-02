# fit-distributions Specification

## Purpose

Fit species-sensitivity distribution models to each simulated dataset produced by the simulate-data capability, expanding a tibble of simulated data into the full factorial grid of distribution-fitting configurations (shape bounds, mixture proportions, rescaling, etc.) so downstream hazard-concentration estimation can operate over all combinations.

## Requirements

### Requirement: Fit distributions to nested simulations
The package SHALL expose `ssd_fit_dists_sims(x, ...)` that accepts a tibble with integer `sim` and `stream` columns and a list column `data`, and returns the tibble augmented with a list column `fits` holding the fitted distribution objects.

#### Scenario: Basic fitting
- **WHEN** `ssd_fit_dists_sims()` is called with a tibble produced by `ssd_sim_data()`
- **THEN** the returned tibble SHALL contain all original columns plus a `fits` list column whose entries are `fitdists` objects from `ssdtools::ssd_fit_dists()`

#### Scenario: Empty input
- **WHEN** `ssd_fit_dists_sims()` is called with a tibble of zero rows
- **THEN** the function SHALL return the input tibble with an empty `dists` list column added and without raising an error

### Requirement: Factorial expansion over fit configurations
The function SHALL cross-join the input tibble with the cartesian product of vector-valued arguments `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, and `range_shape2`, producing one row per (simulation × fit-configuration) combination.

#### Scenario: Multiple rescale values
- **WHEN** `ssd_fit_dists_sims()` is called with `rescale = c(TRUE, FALSE)` on an input of `N` simulations
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, rescale) pair

#### Scenario: Multiple shape ranges
- **WHEN** `range_shape1` is supplied as a list of more than one numeric pair
- **THEN** the returned tibble SHALL include one row per (simulation, range_shape1) combination

### Requirement: Default fit configuration matches ssdtools
Default argument values SHALL mirror the `ssdtools::ssd_fit_dists()` defaults relevant to simulation use, specifically `dists = ssdtools::ssd_dists_bcanz()`, `rescale = FALSE`, `computable = FALSE`, `at_boundary_ok = TRUE`, `min_pmix = list(ssdtools::ssd_min_pmix)`, `range_shape1 = list(c(0.05, 20))`, and `range_shape2 = range_shape1`.

#### Scenario: Omitted arguments use defaults
- **WHEN** `ssd_fit_dists_sims()` is called with only `x`
- **THEN** the function SHALL fit distributions from `ssdtools::ssd_dists_bcanz()` without rescaling, allowing parameters at boundaries, with shape-parameter bounds of `[0.05, 20]`

### Requirement: Argument validation
The function SHALL validate each argument and abort with an informative error on invalid input.

#### Scenario: Missing data column
- **WHEN** the input tibble lacks a `data` column
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an error

#### Scenario: Invalid min_pmix
- **WHEN** `min_pmix` is not a list of single-argument functions
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an error

#### Scenario: Invalid shape range length
- **WHEN** `range_shape1` contains a numeric vector of length other than 2
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an error

### Requirement: Reproducible fitting
Given the same input tibble and the same `seed`, the function SHALL produce identical `fits` output across invocations.

#### Scenario: Deterministic fits
- **WHEN** `ssd_fit_dists_sims()` is invoked twice with the same `x` and same `seed`
- **THEN** the resulting `fits` columns SHALL be identical

### Requirement: Silent fitting by default
The function SHALL suppress `ssdtools::ssd_fit_dists()` warnings and messages by default, controlled by a `silent` flag defaulting to `TRUE`.

#### Scenario: Silent default
- **WHEN** `ssd_fit_dists_sims()` is called without specifying `silent`
- **THEN** warnings from individual distribution fits SHALL be suppressed

#### Scenario: Opt-in verbose
- **WHEN** `ssd_fit_dists_sims()` is called with `silent = FALSE`
- **THEN** the function SHALL propagate warnings from failed fits to the user
