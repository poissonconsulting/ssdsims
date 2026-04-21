# simulate-data Specification

## Purpose

Generate tibbles of nested simulated Species Sensitivity Distribution (SSD) datasets from diverse sources — existing data frames, fitted distribution objects, named R functions, or arbitrary random-generation functions — so downstream SSD fitting and hazard-concentration steps can operate on reproducible, parameterized simulation inputs.

## Requirements

### Requirement: Dispatch by input type
The package SHALL expose a single user-facing generic `ssd_sim_data(x, ...)` that dispatches to the appropriate method based on the class of `x`, supporting `data.frame`, `fitdists`, `tmbfit`, `character`, and `function` inputs.

#### Scenario: Data frame dispatch
- **WHEN** `ssd_sim_data()` is called with a data frame containing a `Conc` column
- **THEN** the system SHALL dispatch to the `data.frame` method and generate samples by drawing rows from `x`

#### Scenario: Function dispatch
- **WHEN** `ssd_sim_data()` is called with a random-number-generating function such as `ssdtools::ssd_rlnorm`
- **THEN** the system SHALL dispatch to the `function` method and generate samples by calling the function with `n = nrow` and the supplied `args`

#### Scenario: Character dispatch
- **WHEN** `ssd_sim_data()` is called with a character string naming an R function (e.g., `"rnorm"`)
- **THEN** the system SHALL parse and evaluate the string to resolve the function, then dispatch to the `function` method

#### Scenario: fitdists dispatch
- **WHEN** `ssd_sim_data()` is called with a `fitdists` object
- **THEN** the system SHALL select the generating distribution according to `dist_sim` (a named distribution, `"top"` for the highest-weighted distribution, `"multi"` for the model-averaged multi-distribution, or `"all"` to expand to every distribution in the object) and delegate generation to the underlying `tmbfit` or multi-distribution sampler

#### Scenario: tmbfit dispatch
- **WHEN** `ssd_sim_data()` is called with a `tmbfit` object
- **THEN** the system SHALL extract the fitted parameter estimates and the distribution name, and delegate to the `character`/`function` method using `ssdtools::ssd_r<dist>`

### Requirement: Nested tibble output
Every method SHALL return a tibble with one row per simulation configuration, containing at minimum integer `sim` and `stream` columns and a list column `data` holding the simulated dataset tibbles.

#### Scenario: Single-configuration output
- **WHEN** the user requests `nsim` simulations with scalar `nrow` and `replace` arguments from a data frame source
- **THEN** the returned tibble SHALL have `nsim` rows and columns `sim`, `stream`, `nrow`, `replace`, `data`

#### Scenario: Expanded configuration output
- **WHEN** the user supplies vectors for `nrow` and/or `replace` (or `dist_sim`)
- **THEN** the returned tibble SHALL contain the full cross product of configurations, each with its own nested simulated data set

### Requirement: Input validation
Methods SHALL validate user-supplied arguments and abort with an informative error when inputs are outside the supported domain.

#### Scenario: Invalid nrow range
- **WHEN** `nrow` contains a value outside the closed range `[5, 1000]`
- **THEN** `ssd_sim_data()` SHALL abort with an error identifying the offending argument

#### Scenario: Unknown dist_sim value
- **WHEN** `dist_sim` for the `fitdists` method contains a value that is not `"all"`, `"multi"`, `"top"`, or a distribution present in the `fitdists` object
- **THEN** `ssd_sim_data()` SHALL abort with an error

#### Scenario: Conc column required
- **WHEN** the `data.frame` method is called with a data frame lacking a `Conc` column
- **THEN** `ssd_sim_data()` SHALL abort with an error

### Requirement: Reproducible stream-aware seeding
Methods SHALL produce identical output for identical combinations of `seed`, `stream`, `start_sim`, and `nsim`, using the L'Ecuyer-CMRG parallel-safe RNG streams provided by the seeding capability.

#### Scenario: Same seed, same stream
- **WHEN** `ssd_sim_data()` is called twice with the same `seed`, `stream`, and `start_sim`
- **THEN** both calls SHALL return identical simulated datasets

#### Scenario: Different streams
- **WHEN** `ssd_sim_data()` is called with the same `seed` but different `stream` values
- **THEN** the calls SHALL return different simulated datasets drawn from non-overlapping sub-streams

### Requirement: Global stream default
The `stream` argument SHALL default to the value of the `ssdsims.stream` R option, falling back to `1L` if the option is unset.

#### Scenario: Option overrides default
- **WHEN** `options(ssdsims.stream = 3L)` is set and `ssd_sim_data()` is called without an explicit `stream`
- **THEN** the system SHALL use stream `3L`
