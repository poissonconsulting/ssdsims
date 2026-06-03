## MODIFIED Requirements

### Requirement: Dataset input (single or list)
`ssd_define_scenario()` SHALL accept datasets as either a single input or a list of inputs, and SHALL derive or accept a dataset name for each. Each input SHALL be one of: a data frame, a `fitdists` object, a `tmbfit` object, a generator function (single argument, the number of rows), or a function-name string — the same set of inputs `ssd_run_scenario()` accepts through the `ssd_sim_data()` S3 methods. A non-data-frame input is a data *generator* that SHALL be **materialised once, at construction**, to a validated tibble (with a numeric `Conc` column) stored inline under its derived name, indistinguishable downstream from a data-frame input.

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

#### Scenario: Named list with an explicit name is rejected
- **WHEN** both a named list and an explicit `name=` are supplied
- **THEN** the constructor SHALL abort with an informative error signalling the conflict (matching the current `scenario_dataset_names()` behaviour)

#### Scenario: fitdists object materialised as a dataset
- **WHEN** `ssd_define_scenario(fit, ..., .seed = 1L)` is called where `fit` is a `fitdists` object
- **THEN** the scenario SHALL derive the dataset name from the argument (e.g. `"fit"`) and store, under that name, the tibble produced by materialising the generator once at construction

#### Scenario: tmbfit object materialised as a dataset
- **WHEN** `ssd_define_scenario(fit[[1]], name = "lnorm", ..., .seed = 1L)` is called where the input is a single `tmbfit`
- **THEN** the scenario SHALL store, under the supplied (or derived) name, the tibble produced by materialising the `tmbfit` generator once at construction

#### Scenario: Generator function materialised as a dataset
- **WHEN** `ssd_define_scenario(ssdtools::ssd_rlnorm, ..., .seed = 1L)` is called with a generator function
- **THEN** the scenario SHALL derive the dataset name from the argument expression (e.g. `"ssd_rlnorm"`) and store, under that name, the tibble produced by running the generator once at construction

#### Scenario: Function-name string materialised as a dataset
- **WHEN** `ssd_define_scenario("ssd_rlnorm", ..., .seed = 1L)` is called with a function-name string
- **THEN** the scenario SHALL use that string as the dataset name, materialise the resolved function once, and SHALL abort with an informative error if the string does not resolve to a function

#### Scenario: Mixed list of data frames and generators
- **WHEN** `ssd_define_scenario(list(boron = ssddata::ccme_boron, synth = ssdtools::ssd_rlnorm), ..., .seed = 1L)` is called
- **THEN** the scenario SHALL store `boron` as the given data frame and `synth` as the tibble materialised from the generator, each under its list name, with unique names enforced across the collection

### Requirement: Input data assembly and normalisation
The package SHALL expose `ssd_data()` as the single entry point that assembles one or more inputs into a validated, named collection — an `ssdsims_data` object that is a homogeneous named list of tibbles. Each element SHALL be a tibble: a data-frame input passed through `Conc` validation, or the tibble produced by **materialising a generator input once at construction**. `ssd_define_scenario()` SHALL accept either an `ssd_data()` collection or, for convenience, bare input(s) routed through the same per-element handling. Every element SHALL be required to have a numeric `Conc` column, with additional columns preserved. Names SHALL be taken from argument names where supplied, otherwise derived by symbol capture, and SHALL be unique across the collection.

#### Scenario: Conc column required
- **WHEN** `ssd_data()` (or `ssd_define_scenario()`) is given an input whose materialised tibble lacks a numeric `Conc` column
- **THEN** the function SHALL abort with an informative error

#### Scenario: Valid data frame passes through as a tibble element
- **WHEN** `ssd_data()` is given one or more data frames with a numeric `Conc` column
- **THEN** the collection SHALL hold those elements as tibbles preserving the `Conc` column and any additional columns

#### Scenario: Generator inputs materialised into the collection
- **WHEN** `ssd_data(boron = ssddata::ccme_boron, gen = ssdtools::ssd_rlnorm, refit = fit, .seed = 1L)` is called (with `fit` a `fitdists` object)
- **THEN** the collection SHALL hold `boron` as the given tibble and `gen`/`refit` as the tibbles produced by materialising the generators once at construction

#### Scenario: Names derived and unique across mixed inputs
- **WHEN** `ssd_data(ssddata::ccme_boron, ssdtools::ssd_rlnorm, .seed = 1L)` is called with unnamed inputs
- **THEN** names SHALL be derived by symbol capture (`"ccme_boron"`, `"ssd_rlnorm"`) and duplicate names across the collection SHALL abort with an informative error

## ADDED Requirements

### Requirement: Generators materialised at construction under a dedicated, scenario-independent seed
`ssd_data()` and `ssd_define_scenario()` SHALL materialise each non-data-frame input once, at construction, to an inline tibble, seeded **independently of the scenario `seed`** by a dedicated dot-prefixed `ssd_data(.seed = NULL)` argument. Each generator SHALL be run under a scoped RNG state derived from `.seed` as the base seed and the dataset **name** as the stream, so a single `.seed` fans out across all named generators on independent streams. The generation seeding SHALL NOT enter the per-task simulation primers. The construction SHALL leave the global RNG state (`.Random.seed`) unchanged on return.

#### Scenario: Reproducible generation under `.seed`
- **WHEN** two scenarios are constructed from the same generator and the same `.seed`
- **THEN** the materialised dataset SHALL be byte-identical; a different `.seed` SHALL (for an RNG-using generator) yield different data

#### Scenario: One `.seed` across several generators
- **WHEN** `ssd_data()` is given several generators and a single `.seed`
- **THEN** each generator SHALL draw on an independent stream keyed by its name, and the result SHALL be reproducible

#### Scenario: `.seed` without generators is rejected
- **WHEN** `.seed` is supplied but the call contains no generator inputs
- **THEN** the function SHALL abort with an informative error

#### Scenario: Global RNG state preserved
- **WHEN** `ssd_define_scenario(ssdtools::ssd_rlnorm, ..., .seed = 1L)` is called
- **THEN** `.Random.seed` SHALL be unchanged after the call (the scoped generation restores it)

### Requirement: dqrng-only, pure generators enforced post hoc
`ssd_data()` and `ssd_define_scenario()` SHALL detect, by comparing RNG state before and after each generator runs, whether the generator consumed randomness, and SHALL enforce a dqrng-only contract: a generator that moves the base R `.Random.seed` SHALL abort (directing the user to dqrng); a generator that moves the dqrng state with `.seed = NULL` SHALL abort (directing the user to supply `.seed`); a pure generator SHALL pass with or without `.seed`.

#### Scenario: base R RNG in a generator is rejected
- **WHEN** a generator draws from the base R RNG (moves `.Random.seed`)
- **THEN** the function SHALL abort with an informative error directing the user to use dqrng

#### Scenario: dqrng generator without a seed is rejected
- **WHEN** a generator draws from dqrng and `.seed` is `NULL`
- **THEN** the function SHALL abort with an informative error directing the user to supply `.seed`

#### Scenario: pure generator needs no seed
- **WHEN** a generator consumes no randomness
- **THEN** it SHALL materialise successfully whether or not `.seed` is supplied

### Requirement: Structural validation of generator inputs
`ssd_define_scenario()` and `ssd_data()` SHALL validate generator inputs structurally, in the context of the user-facing function, and abort with an informative error on invalid input. A function-name string SHALL resolve to a function; a generator function SHALL be a function; a name SHALL be derivable for every input (or supplied explicitly).

#### Scenario: Unresolvable function-name string
- **WHEN** a function-name string is supplied that does not resolve to a function in scope
- **THEN** the function SHALL abort with an informative error naming the offending string

#### Scenario: Underivable generator name requires explicit name
- **WHEN** a generator input has no derivable name (e.g. an anonymous function literal) and no `name=`/list name is supplied
- **THEN** the function SHALL abort with an informative error directing the user to supply a name
