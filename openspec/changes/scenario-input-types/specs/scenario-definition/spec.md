## MODIFIED Requirements

### Requirement: Input data assembly and normalisation
The package SHALL expose `ssd_scenario_data()` (renamed from `ssd_data()`, which collided with the unrelated `ssdtools::ssd_data(x)`) as the single entry point that assembles one or more inputs into a validated, named collection — an `ssdsims_data` object that is a homogeneous named list of tibbles. Each element SHALL be a tibble with a numeric `Conc` column (additional columns preserved): a data-frame input passed through `Conc` validation, or a tibble produced by `ssd_gen()`. `ssd_scenario_data()` SHALL accept the result of `ssd_gen()` (an `ssdsims_gen` object) both as an unnamed argument (its members flattened into the collection) and via `rlang` splicing (`!!!ssd_gen(...)`), with identical results. Names SHALL be taken from argument names where supplied, otherwise derived by symbol capture, and SHALL be unique across the collection.

#### Scenario: Conc column required
- **WHEN** `ssd_scenario_data()` is given an input whose tibble lacks a numeric `Conc` column
- **THEN** the function SHALL abort with an informative error

#### Scenario: Valid data frame passes through as a tibble element
- **WHEN** `ssd_scenario_data()` is given one or more data frames with a numeric `Conc` column
- **THEN** the collection SHALL hold those elements as tibbles preserving the `Conc` column and any additional columns

#### Scenario: An ssd_gen() result is flattened into the collection
- **WHEN** `ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))` is called with the `ssd_gen()` result as an unnamed argument
- **THEN** the collection SHALL hold `boron` as the given tibble and `synth` as the materialised generator tibble, each under its name

#### Scenario: Splicing an ssd_gen() result is equivalent
- **WHEN** the same call is written `ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L))`
- **THEN** the resulting collection SHALL be identical to the unnamed-argument form

#### Scenario: Names derived and unique across mixed inputs
- **WHEN** `ssd_scenario_data(ccme_boron, !!!ssd_gen(ssd_rlnorm, .n = 30, .seed = 1L))` is called with names derived by symbol capture
- **THEN** names SHALL be derived (`"ccme_boron"`, `"ssd_rlnorm"`) and duplicate names across the collection SHALL abort with an informative error

## ADDED Requirements

### Requirement: Dataset input via an ssd_scenario_data() collection
`ssd_define_scenario()` SHALL accept dataset input ONLY as an `ssd_scenario_data()` collection (an `ssdsims_data` object). Bare data frames, bare lists, and a `name=` argument SHALL NOT be accepted; naming and validation are owned by `ssd_scenario_data()`/`ssd_gen()`. Because generation is performed by `ssd_gen()` before construction, `ssd_define_scenario()` SHALL perform no random-number generation (the "No side effects on RNG state" requirement is preserved).

#### Scenario: Collection accepted
- **WHEN** `ssd_define_scenario(ssd_scenario_data(boron = ccme_boron), nsim = 100L, nrow = c(5L, 10L), seed = 42L)` is called
- **THEN** the scenario SHALL store `boron` as its sole dataset name and the validated tibble in `$data`

#### Scenario: Collection with a generated dataset accepted
- **WHEN** `ssd_define_scenario(ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 1L)), nsim = 100L, seed = 42L)` is called
- **THEN** the scenario SHALL store `c("boron", "synth")` as dataset names and the materialised tibbles in `$data`, indistinguishable downstream from data-frame datasets

#### Scenario: Bare data frame rejected
- **WHEN** `ssd_define_scenario()` is given a bare data frame (or list, or a `name=` argument) instead of an `ssd_scenario_data()` collection
- **THEN** the constructor SHALL abort with an informative error directing the user to `ssd_scenario_data()`

#### Scenario: Construction draws no random numbers
- **WHEN** `ssd_define_scenario()` is called with any `ssd_scenario_data()` collection
- **THEN** the global RNG state (`.Random.seed`) SHALL be unchanged after the call returns

### Requirement: Generator materialisation via ssd_gen()
The package SHALL expose `ssd_gen(..., .n, .seed)` that accepts ONLY generator-style inputs — a function, a function-name string, a `fitdists` object, or a `tmbfit` object — and materialises each, once, to a validated tibble with a numeric `Conc` column of `.n` rows, returning a classed `ssdsims_gen` named collection suitable for use within (or splicing into) `ssd_scenario_data()`. `.n` and `.seed` SHALL be required, dot-prefixed formals (never absorbed into `...`, never partial-matched from a `seed=`/`n=` named generator). Dispatch SHALL be most-specific-first (`tmbfit` before `fitdists`); a function-name string SHALL be resolved to a function via a bare-name lookup (`get0()`/`match.fun()`, no `eval(parse())`) and SHALL also be the dataset name; `fitdists`/`tmbfit` SHALL resolve to the matching `ssd_r<dist>` draw via `ssdtools::estimates()`. A `data.frame` SHALL be rejected (it belongs in `ssd_scenario_data()`). Each generator SHALL be materialised under a scoped dqrng state seeded by `.seed` (base seed) with the dataset **name** as the dqrng stream (`task_primer(list(dataset = name))`), so one `.seed` fans out across all generators on independent streams; the global `.Random.seed` SHALL be unchanged on return.

#### Scenario: Function generator materialised
- **WHEN** `ssd_gen(synth = ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)` is called
- **THEN** the result SHALL be an `ssdsims_gen` collection holding `synth` as a tibble of 30 rows with a numeric `Conc` column

#### Scenario: Function-name string resolved and materialised
- **WHEN** `ssd_gen("ssd_rlnorm", .n = 30, .seed = 1L)` is called
- **THEN** the string SHALL resolve to the function, be used as the dataset name, and produce a tibble identical to the function form

#### Scenario: tmbfit and fitdists materialised
- **WHEN** `ssd_gen(refit = fit[[1]], .n = 30, .seed = 1L)` (a `tmbfit`) or `ssd_gen(refit = fit, .n = 30, .seed = 1L)` (a `fitdists`, top-weighted dist selected) is called
- **THEN** each SHALL materialise a `Conc` tibble drawn from the matching `ssd_r<dist>` with the fit's estimates

#### Scenario: Reproducible under .seed
- **WHEN** `ssd_gen()` is called twice with the same generator and the same `.seed`
- **THEN** the materialised tibble SHALL be byte-identical; a different `.seed` SHALL (for an RNG-using generator) yield different data

#### Scenario: One .seed across several generators
- **WHEN** `ssd_gen(a = ssd_rlnorm, b = ssd_rlnorm, .n = 30, .seed = 1L)` is called
- **THEN** each generator SHALL draw on an independent stream keyed by its name (so `a` and `b` differ), and the result SHALL be reproducible

#### Scenario: .seed and .n are required
- **WHEN** `ssd_gen()` is called without `.seed` (or without `.n`)
- **THEN** the function SHALL abort with an informative error

#### Scenario: Data frame rejected by ssd_gen()
- **WHEN** `ssd_gen(d = ccme_boron, .n = 30, .seed = 1L)` is called with a data frame
- **THEN** the function SHALL abort with an informative error directing the user to `ssd_scenario_data()`

#### Scenario: Global RNG state preserved
- **WHEN** `ssd_gen(ssdtools::ssd_rlnorm, .n = 30, .seed = 1L)` is called
- **THEN** `.Random.seed` SHALL be unchanged after the call (the scoped generation restores it)

### Requirement: dqrng-backed, reproducible generation
`ssd_gen()` SHALL draw under an active dqrng pcg64 backend and SHALL reuse `task-rng-postcheck`'s per-task integrity witness to enforce that each draw actually came from dqrng. Each generator SHALL be seeded through `local_dqrng_state()`, which brackets the draw with `chk_dqrng_backend_intact()`; `ssd_gen()` SHALL abort when a generator escaped dqrng (e.g. drew from base R after switching `RNGkind`), since such a draw is not reproducible under `.seed`. A generator that consumes no randomness SHALL pass.

#### Scenario: A generator escaping dqrng is rejected
- **WHEN** a generator draws from the base R RNG (escaping the dqrng backend)
- **THEN** the function SHALL abort with an informative error (the draw is not reproducible under `.seed`)

#### Scenario: A pure generator passes
- **WHEN** a generator consumes no randomness
- **THEN** it SHALL materialise successfully under any `.seed`

### Requirement: Structural validation of generator inputs
`ssd_gen()` SHALL validate generator inputs structurally, in the context of the user-facing function, and abort with an informative error on invalid input. A function-name string SHALL resolve to a function; a generator function SHALL be a function; a name SHALL be derivable for every input (argument name, the string itself, or symbol capture) or supplied explicitly.

#### Scenario: Unresolvable function-name string
- **WHEN** a function-name string is supplied that does not resolve to a function in scope
- **THEN** the function SHALL abort with an informative error naming the offending string

#### Scenario: Underivable generator name requires explicit name
- **WHEN** a generator input has no derivable name (e.g. an anonymous function literal) and no argument name is supplied
- **THEN** the function SHALL abort with an informative error directing the user to supply a name

## REMOVED Requirements

### Requirement: Dataset input (single or list)
**Reason**: `ssd_define_scenario()` no longer accepts bare data frames, bare lists, or a `name=` argument. All dataset input now flows through an `ssd_scenario_data()` collection (data frames) composed with `ssd_gen()` (generators), which own naming and validation. Restated as *"Dataset input via an ssd_scenario_data() collection"*.
