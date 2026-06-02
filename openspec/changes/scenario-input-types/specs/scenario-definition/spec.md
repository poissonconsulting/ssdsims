## MODIFIED Requirements

### Requirement: Dataset input (single or list)
`ssd_define_scenario()` SHALL accept datasets as either a single input or a list of inputs, and SHALL derive or accept a dataset name for each. Each input SHALL be one of: a data frame, a `fitdists` object, a `tmbfit` object, a generator function (single argument, the number of rows), or a function-name string — the same set of inputs `ssd_run_scenario()` accepts through the `ssd_sim_data()` S3 methods. Non-data-frame inputs are recorded declaratively as data *generators* (by name and kind); they SHALL NOT be executed by the constructor.

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

#### Scenario: fitdists object as a generator
- **WHEN** `ssd_define_scenario(fit, ...)` is called where `fit` is a `fitdists` object
- **THEN** the scenario SHALL derive the dataset name from the argument (e.g. `"fit"`), record the dataset as a generator of kind `fitdists`, and store no `fitdists` payload

#### Scenario: tmbfit object as a generator
- **WHEN** `ssd_define_scenario(fit[[1]], name = "lnorm", ...)` is called where the input is a single `tmbfit`
- **THEN** the scenario SHALL record the dataset as a generator of kind `tmbfit` under the supplied (or derived) name, storing no `tmbfit` payload

#### Scenario: Generator function captured by name
- **WHEN** `ssd_define_scenario(ssdtools::ssd_rlnorm, ...)` is called with a generator function
- **THEN** the scenario SHALL derive the dataset name from the argument expression (e.g. `"ssd_rlnorm"`), record the dataset as a generator of kind `function`, and store no function body

#### Scenario: Function-name string as a generator
- **WHEN** `ssd_define_scenario("ssd_rlnorm", ...)` is called with a function-name string
- **THEN** the scenario SHALL use that string as the generator name, record the dataset as a generator of kind `function`, and SHALL abort with an informative error if the string does not resolve to a function

#### Scenario: Mixed list of data frames and generators
- **WHEN** `ssd_define_scenario(list(boron = ssddata::ccme_boron, synth = ssdtools::ssd_rlnorm), ...)` is called
- **THEN** the scenario SHALL record `boron` as a data-frame dataset and `synth` as a generator dataset, each under its list name, with unique names enforced across the collection

### Requirement: Input data assembly and normalisation
The package SHALL expose `ssd_data()` as the single entry point that assembles one or more inputs into a validated, named collection — an `ssdsims_data` object. Each element SHALL be either an inline data-frame dataset (a tibble) or a declarative generator descriptor (name + kind + captured reference). `ssd_define_scenario()` SHALL accept either an `ssd_data()` collection or, for convenience, bare input(s) routed through the same per-element handling. A data-frame element SHALL be required to have a numeric `Conc` column, with additional columns preserved; a generator element SHALL be validated structurally without being executed. Names SHALL be taken from argument names where supplied, otherwise derived by symbol capture, and SHALL be unique across the collection.

A generator element SHALL store no function body and no `fitdists`/`tmbfit` payload; the actual data is materialised later by the targets-only `dataset-registry` change (`TARGETS-DESIGN.md` §1.1).

#### Scenario: Conc column required for data frames
- **WHEN** `ssd_data()` (or `ssd_define_scenario()`) is given a data-frame input lacking a numeric `Conc` column
- **THEN** the function SHALL abort with an informative error

#### Scenario: Valid data frame passes through as a tibble element
- **WHEN** `ssd_data()` is given one or more data frames with a numeric `Conc` column
- **THEN** the collection SHALL hold those elements as tibbles preserving the `Conc` column and any additional columns

#### Scenario: Generator inputs accepted and described
- **WHEN** `ssd_data(boron = ssddata::ccme_boron, gen = ssdtools::ssd_rlnorm, refit = fit)` is called (with `fit` a `fitdists` object)
- **THEN** the collection SHALL hold `boron` as a tibble and `gen`/`refit` as generator descriptors recording kind `function` and `fitdists` respectively, without running the generators

#### Scenario: Names derived and unique across mixed inputs
- **WHEN** `ssd_data(ssddata::ccme_boron, ssdtools::ssd_rlnorm)` is called with unnamed inputs
- **THEN** names SHALL be derived by symbol capture (`"ccme_boron"`, `"ssd_rlnorm"`) and duplicate names across the collection SHALL abort with an informative error

## ADDED Requirements

### Requirement: Generators recorded declaratively, never executed at construction
`ssd_data()` and `ssd_define_scenario()` SHALL record a non-data-frame input as a generator descriptor carrying its name and a generator kind (`fitdists`, `tmbfit`, or `function`), and SHALL NOT run the generator, draw any random numbers, or store a function body or model payload at construction time. The constructor SHALL leave the global RNG state (`.Random.seed`) unchanged when given generator inputs.

#### Scenario: No RNG draws for generator input
- **WHEN** `ssd_define_scenario(ssdtools::ssd_rlnorm, nsim = 10L, nrow = 5L, seed = 1L)` is called
- **THEN** `.Random.seed` SHALL be unchanged after the call and no data SHALL be generated

#### Scenario: No function body or payload stored
- **WHEN** a scenario is constructed from a generator function or a `fitdists`/`tmbfit` object
- **THEN** the stored descriptor SHALL contain only the name and kind (plus a symbol/string reference), and SHALL NOT contain the function body or the model object

### Requirement: Structural validation of generator inputs
`ssd_define_scenario()` and `ssd_data()` SHALL validate generator inputs structurally, in the context of the user-facing function, and abort with an informative error on invalid input. A function-name string SHALL resolve to a function; a generator function SHALL be a function; a name SHALL be derivable for every input (or supplied explicitly).

#### Scenario: Unresolvable function-name string
- **WHEN** a function-name string is supplied that does not resolve to a function in scope
- **THEN** the function SHALL abort with an informative error naming the offending string

#### Scenario: Underivable generator name requires explicit name
- **WHEN** a generator input has no derivable name (e.g. an anonymous function literal) and no `name=`/list name is supplied
- **THEN** the function SHALL abort with an informative error directing the user to supply a name
