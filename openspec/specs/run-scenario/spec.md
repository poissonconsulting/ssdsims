# run-scenario Specification

## Purpose

Provide a single entry point that runs an end-to-end SSD simulation scenario — data simulation, distribution fitting, and hazard-concentration estimation — from any supported input source, chaining the three underlying capabilities with a unified argument surface.

## Requirements

### Requirement: Unified scenario dispatch
The package SHALL expose a generic `ssd_run_scenario(x, ...)` that dispatches to methods for `data.frame`, `fitdists`, `tmbfit`, `character`, and `function` inputs, mirroring the dispatch table of `ssd_sim_data()`.

#### Scenario: Data frame source
- **WHEN** `ssd_run_scenario()` is called with a data frame
- **THEN** the system SHALL sample simulated data from the data frame, fit distributions, compute hazard concentrations, and return a single combined tibble

#### Scenario: Fitted distribution source
- **WHEN** `ssd_run_scenario()` is called with a `fitdists` object and `dist_sim`
- **THEN** the system SHALL generate simulated data from the selected distribution(s), fit distributions, and compute hazard concentrations

#### Scenario: Character source
- **WHEN** `ssd_run_scenario()` is called with a character string naming an R function (e.g., `"rlnorm"`)
- **THEN** the system SHALL evaluate the string to resolve the function, then delegate to the `function` method

#### Scenario: tmbfit source
- **WHEN** `ssd_run_scenario()` is called with a `tmbfit` object
- **THEN** the system SHALL extract the fitted parameter estimates and distribution name and delegate to the `character` method with `ssdtools::ssd_r<dist>`

### Requirement: End-to-end pipeline semantics
The generic SHALL produce the same tibble as calling `ssd_sim_data()`, `ssd_fit_dists_sims()`, and `ssd_hc_sims()` in sequence with the corresponding arguments.

#### Scenario: Pipeline equivalence
- **WHEN** a user runs `ssd_run_scenario(x, nsim = N, ...)`
- **THEN** the output SHALL be equivalent to `ssd_sim_data(x, nsim = N, ...) |> ssd_fit_dists_sims(...) |> ssd_hc_sims(...)` with the corresponding arguments routed to each stage

### Requirement: Unified argument surface
Arguments accepted by the underlying simulate/fit/HC steps SHALL be accepted at the scenario level and forwarded to the appropriate step.

#### Scenario: Fitting arguments forwarded
- **WHEN** `ssd_run_scenario()` is called with `dists`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, or `range_shape2`
- **THEN** those arguments SHALL be forwarded to the distribution-fitting step

#### Scenario: HC arguments forwarded
- **WHEN** `ssd_run_scenario()` is called with `proportion`, `ci`, `nboot`, `est_method`, `ci_method`, or `parametric`
- **THEN** those arguments SHALL be forwarded to the hazard-concentration step

#### Scenario: Simulation arguments forwarded
- **WHEN** `ssd_run_scenario()` is called with `nrow`, `replace`, `nsim`, `stream`, `start_sim`, `seed`, or `args`
- **THEN** those arguments SHALL be forwarded to the data-simulation step

### Requirement: Progress reporting
The function SHALL accept a `.progress` flag that, when `TRUE`, enables a progress bar for the long-running simulation, fitting, and bootstrapping steps.

#### Scenario: Progress disabled by default
- **WHEN** `ssd_run_scenario()` is called without `.progress`
- **THEN** no progress bar SHALL be shown
