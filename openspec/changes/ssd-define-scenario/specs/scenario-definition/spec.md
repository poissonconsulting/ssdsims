## ADDED Requirements

### Requirement: Declarative scenario constructor
The package SHALL expose `ssd_define_scenario()` returning an S3 object of class `ssdsims_scenario` that stores only declarative fields and performs no random-number generation, no task expansion, and no `targets` interaction.

#### Scenario: Construct a minimal scenario
- **WHEN** `ssd_define_scenario(data, nsim = 100L, nrow = c(5L, 10L), seed = 42L)` is called with a valid dataset
- **THEN** the function SHALL return an `ssdsims_scenario` object carrying `seed`, `nsim`, `nrow`, the dataset name(s), and the `fit`/`hc` argument grids, without drawing any random numbers

#### Scenario: No side effects on RNG state
- **WHEN** `ssd_define_scenario()` is called
- **THEN** the global RNG state (`.Random.seed`) SHALL be unchanged after the call returns

### Requirement: Declarative-only field set
The `ssdsims_scenario` object SHALL store `seed` (a scalar integer), `nsim`, `nrow`, dataset names, the `fit` and `hc` argument-vector grids, `partition_by`, and an optional `upload` spec, and SHALL NOT store materialized task tables, RNG states, or function-body values for name-referenced parameters.

#### Scenario: Seed is a scalar integer
- **WHEN** a scenario is constructed with `seed = 42L`
- **THEN** the object SHALL store `seed` as a single integer that fully re-roots the scenario's RNG when changed

#### Scenario: partition_by defaults are populated
- **WHEN** `ssd_define_scenario()` is called without an explicit `partition_by`
- **THEN** the object SHALL carry the documented per-step defaults (data, fit, hc path axes)

#### Scenario: Upload defaults to none
- **WHEN** `ssd_define_scenario()` is called without an `upload` argument
- **THEN** the object's `upload` field SHALL be `NULL`

### Requirement: Dataset input (single or list)
`ssd_define_scenario()` SHALL accept datasets as either a single data frame or a list of data frames, and SHALL derive or accept dataset names for each.

#### Scenario: Single data frame, implicit name
- **WHEN** `ssd_define_scenario(ssddata::ccme_boron, ...)` is called with a single data frame as the first argument
- **THEN** the scenario SHALL derive the dataset name from the argument name (e.g., `"ccme_boron"` from the symbol or the variable name) and store it as the sole dataset name

#### Scenario: Single data frame, explicit name
- **WHEN** `ssd_define_scenario(ssddata::ccme_boron, name = "boron_data", ...)` is called
- **THEN** the scenario SHALL use the explicit `name` parameter as the dataset name, overriding automatic derivation

#### Scenario: List of data frames with implicit names
- **WHEN** `ssd_define_scenario(list(boron = ssddata::ccme_boron, cadmium = ssddata::ccme_cadmium), ...)` is called with a named list
- **THEN** the scenario SHALL use the list names as the dataset names and store them all (e.g., `c("boron", "cadmium")`)

#### Scenario: List of data frames with derived names
- **WHEN** `ssd_define_scenario(list(ssddata::ccme_boron, ssddata::ccme_cadmium), ...)` is called with an unnamed list
- **THEN** the scenario SHALL derive names from the data frame arguments (e.g., `c("ccme_boron", "ccme_cadmium")`)

#### Scenario: Named list overrides explicit name parameter
- **WHEN** both a named list and an explicit `name=` are supplied
- **THEN** the scenario SHALL use the list element names and ignore the `name=` parameter; provide a warning or error to signal the conflict

### Requirement: Input data normalisation
The package SHALL expose `ssd_data()` that normalises input data to a validated tibble, and `ssd_define_scenario()` SHALL forward all input data through it. `ssd_data()` SHALL require a `Conc` column and a valid tibble shape.

#### Scenario: Conc column required
- **WHEN** `ssd_data()` (or `ssd_define_scenario()`) is given a data frame lacking a `Conc` column
- **THEN** the function SHALL abort with an informative error

#### Scenario: Valid data passes through
- **WHEN** `ssd_data()` is given a data frame with a numeric `Conc` column
- **THEN** the function SHALL return a tibble preserving the `Conc` column and any additional columns

### Requirement: ci = FALSE rejects bootstrap-only knobs
When `ci = FALSE` is the only confidence-interval setting, `ssd_define_scenario()` SHALL abort with an error if any bootstrap-only knobs (`nboot`, `ci_method`, or `parametric`) are passed, enforcing that the user either omit those knobs or explicitly set `ci = c(FALSE, TRUE)` to enable bootstrap.

#### Scenario: Bootstrap knobs rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` is called
- **THEN** the function SHALL abort with an informative error stating that bootstrap-only knobs are not allowed when `ci = FALSE`

#### Scenario: User must explicitly enable bootstrap
- **WHEN** the user wants bootstrap confidence intervals
- **THEN** they SHALL set `ci = c(FALSE, TRUE)` (not just `ci = TRUE`) to signal both pointwise and bootstrap estimates; omitting the `ci = FALSE` case reduces the scenario's asymmetry

#### Scenario: Both ci values retains bootstrap knobs
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE), nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap knobs SHALL be retained and no error SHALL be emitted; the `ci = FALSE` row will collapse to NA knobs at task-expansion time (§1.2 of the design)

### Requirement: Argument validation
`ssd_define_scenario()` SHALL validate its declarative arguments and abort with an informative error on invalid input.

#### Scenario: Invalid seed
- **WHEN** `ssd_define_scenario()` is called with a `seed` that is not a scalar whole number
- **THEN** the function SHALL abort with an error

#### Scenario: Invalid nrow range
- **WHEN** `ssd_define_scenario()` is called with an `nrow` value outside the supported `[5, 1000]` range
- **THEN** the function SHALL abort with an error

### Requirement: Scenario print method
The package SHALL provide a `print.ssdsims_scenario()` method that renders the scenario's declarative fields and any recorded ignored-knob notice.

#### Scenario: Print shows declarative fields
- **WHEN** an `ssdsims_scenario` object is printed
- **THEN** the output SHALL show the seed, dataset names, `nsim`, `nrow`, and the fit/hc argument grids

#### Scenario: Print surfaces ignored knobs
- **WHEN** a scenario constructed with `ci = FALSE` and explicit bootstrap knobs is printed
- **THEN** the output SHALL indicate that the bootstrap-only knobs were ignored
