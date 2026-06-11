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

### Requirement: min_pmix referenced by name
`ssd_define_scenario()` SHALL store `min_pmix` in the `fit` grid as one or more names (a character vector), and SHALL NOT store the function value. The constructor SHALL accept `min_pmix` as a character vector of names, or as a function or list of functions whose name is derived by symbol capture (mirroring dataset name derivation). The registry that resolves a name back to a function is out of scope for this change.

#### Scenario: min_pmix function is stored by name
- **WHEN** `ssd_define_scenario(..., min_pmix = ssdtools::ssd_min_pmix)` (or the default) is called
- **THEN** the object's `fit$min_pmix` SHALL be the derived name (e.g. `"ssd_min_pmix"`) as a character vector, and no function value SHALL be stored

#### Scenario: min_pmix names accepted directly
- **WHEN** `ssd_define_scenario(..., min_pmix = c("default", "strict"))` is called
- **THEN** the object SHALL store those names verbatim as `fit$min_pmix`

#### Scenario: min_pmix list of functions derives names
- **WHEN** `ssd_define_scenario(..., min_pmix = list(ssdtools::ssd_min_pmix))` (unnamed) or `list(strict = ssdtools::ssd_min_pmix)` (named) is called
- **THEN** the object SHALL store the derived element name(s) (e.g. `"ssd_min_pmix"`) or the list names (e.g. `"strict"`) respectively; provided functions SHALL be validated before their name is taken

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

### Requirement: Input data assembly and normalisation
The package SHALL expose `ssd_data()` as the single entry point that assembles one or more datasets into a validated, named collection — an `ssdsims_data` object (a named list of tibbles). `ssd_define_scenario()` SHALL accept either an `ssd_data()` collection or, for convenience, bare data frame input routed through the same per-dataset validation. Each dataset SHALL be required to have a numeric `Conc` column; additional columns SHALL be preserved. Dataset names SHALL be taken from argument names where supplied, otherwise derived by symbol capture, and SHALL be unique.

`ssd_data()` is the extensible input point: a later change (`scenario-input-types`, TARGETS-DESIGN.md §12) MAY extend each input to also be a data generator (`fitdists` / `tmbfit` / a function / a function-name string) that `ssd_run_scenario()` accepts; this change is data-frame-only.

#### Scenario: Conc column required
- **WHEN** `ssd_data()` (or `ssd_define_scenario()`) is given a dataset lacking a `Conc` column
- **THEN** the function SHALL abort with an informative error

#### Scenario: Valid data passes through as a collection
- **WHEN** `ssd_data()` is given one or more data frames with a numeric `Conc` column
- **THEN** the function SHALL return an `ssdsims_data` collection whose elements are tibbles preserving the `Conc` column and any additional columns

#### Scenario: Names derived and unique
- **WHEN** `ssd_data(boron = df1, df2)` is called
- **THEN** names SHALL be taken from the argument name (`"boron"`) or by symbol capture for the unnamed argument, and duplicate names SHALL abort with an informative error

### Requirement: ci = FALSE rejects bootstrap-only scenario options
When `ci = FALSE` is the only confidence-interval setting, `ssd_define_scenario()` SHALL abort with an error if any bootstrap-only scenario options (`nboot`, `ci_method`, or `parametric`) are passed, enforcing that the user either omit those options or explicitly set `ci = c(FALSE, TRUE)` to enable bootstrap.

#### Scenario: Bootstrap-only scenario options rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` is called
- **THEN** the function SHALL abort with an informative error stating that bootstrap-only scenario options are not allowed when `ci = FALSE`

#### Scenario: User must explicitly enable bootstrap
- **WHEN** the user wants bootstrap confidence intervals
- **THEN** they SHALL set `ci = c(FALSE, TRUE)` (not just `ci = TRUE`) to signal both pointwise and bootstrap estimates; omitting the `ci = FALSE` case reduces the scenario's asymmetry

#### Scenario: Both ci values retains the bootstrap axes
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE), nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap axes SHALL be retained and no error SHALL be emitted; the `ci = FALSE` row will collapse to NA options at task-expansion time (§1.2 of the design)

### Requirement: Argument validation
`ssd_define_scenario()` SHALL validate its declarative arguments and abort with an informative error on invalid input.

#### Scenario: Seed is required
- **WHEN** `ssd_define_scenario()` is called without a `seed`
- **THEN** the function SHALL abort with an informative error (the seed is the scenario's RNG root and has no default)

#### Scenario: Invalid seed
- **WHEN** `ssd_define_scenario()` is called with a `seed` that is not a scalar whole number
- **THEN** the function SHALL abort with an error

#### Scenario: Invalid nrow range
- **WHEN** `ssd_define_scenario()` is called with an `nrow` value outside the supported `[5, 1000]` range
- **THEN** the function SHALL abort with an error

### Requirement: Scenario print method
The package SHALL provide a `print.ssdsims_scenario()` method that renders the scenario's declarative fields.

#### Scenario: Print shows declarative fields
- **WHEN** an `ssdsims_scenario` object is printed
- **THEN** the output SHALL show the seed, dataset names, `nsim`, `nrow`, and the fit/hc argument grids
