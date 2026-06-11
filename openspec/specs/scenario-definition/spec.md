# scenario-definition Specification

## Purpose

Provide the purely declarative `ssdsims_scenario` object that roots the targets-based pipeline (`TARGETS-DESIGN.md` §1): its constructor `ssd_define_scenario()`, the `ssd_data()` input normaliser, dataset and `min_pmix` name references, field validation, the scalar `ci` flag (with its bootstrap-option rejection when `ci = FALSE`), and the `print()` method. The object stores only declarative fields — never data frames, RNG state, or function bodies — so a scenario serialises to a compact manifest and the shard set a pipeline expands to is a pure function of the scenario.

## Requirements

### Requirement: Declarative scenario constructor
The package SHALL expose `ssd_define_scenario()` returning an S3 object of class `ssdsims_scenario` that stores only declarative fields and performs no random-number generation, no task expansion, and no `targets` interaction.

#### Scenario: Construct a minimal scenario
- **WHEN** `ssd_define_scenario(data, nsim = 100L, nrow = c(5L, 10L), seed = 42L)` is called with a valid dataset
- **THEN** the function SHALL return an `ssdsims_scenario` object carrying `seed`, `nsim`, `nrow`, the dataset name(s), and the `fit`/`hc` argument grids, without drawing any random numbers

#### Scenario: No side effects on RNG state
- **WHEN** `ssd_define_scenario()` is called
- **THEN** the global RNG state (`.Random.seed`) SHALL be unchanged after the call returns

### Requirement: Declarative-only field set
The `ssdsims_scenario` object SHALL store `seed` (a scalar integer), `nsim`, `nrow`, dataset names, the `fit` and `hc` argument-vector grids, and `partition_by`, and SHALL NOT store materialized task tables or RNG states. It SHALL NOT carry an `upload` field: the upload destination is an execution concern supplied to the runner (`ssd_scenario_targets(..., upload = ...)`), not part of the scenario's declarative identity. For name-referenced parameters it additionally carries the **materialised value needed for execution** — the `min_pmix` functions, keyed by name — which are used only when running a task and SHALL NOT enter any task hash (task identities use the names, not the values).

#### Scenario: Seed is a scalar integer
- **WHEN** a scenario is constructed with `seed = 42L`
- **THEN** the object SHALL store `seed` as a single integer that fully re-roots the scenario's RNG when changed

#### Scenario: partition_by defaults are populated
- **WHEN** `ssd_define_scenario()` is called without an explicit `partition_by`
- **THEN** the object SHALL carry the documented per-step defaults (data, fit, hc path axes)

#### Scenario: No upload field on the scenario
- **WHEN** an `ssdsims_scenario` is constructed
- **THEN** the object SHALL NOT contain an `upload` field, and `ssd_define_scenario()` SHALL NOT accept an `upload` argument

#### Scenario: min_pmix functions are carried for execution but not hashed
- **WHEN** a scenario is constructed with a `min_pmix` reference
- **THEN** the object SHALL carry the resolved `min_pmix` function keyed by name for execution, and that function value SHALL NOT contribute to any task hash

### Requirement: min_pmix referenced by name
`ssd_define_scenario()` SHALL store `min_pmix` in the `fit` grid as one or more names (a character vector) — used for hashing and the task path — and SHALL additionally materialise the resolved single-argument **functions**, keyed by name, on the scenario for execution. A supplied function SHALL be stored under its derived name; a name-string reference SHALL be resolved to a function at construction (from `ssdtools` or the caller's environment), failing fast if it cannot be resolved to a single-argument function. The stored names — and therefore task hashes — SHALL be unchanged by the materialisation, and the function values SHALL NOT be hashed. Resolution of a name back to a function is the scenario accessor `scenario_min_pmix()`, not a separate registry.

#### Scenario: min_pmix function is stored by name and materialised
- **WHEN** `ssd_define_scenario(..., min_pmix = ssdtools::ssd_min_pmix)` (or the default) is called
- **THEN** the object's `fit$min_pmix` SHALL be the derived name (e.g. `"ssd_min_pmix"`) as a character vector, and the resolved function SHALL be materialised on the scenario keyed by that name

#### Scenario: min_pmix names accepted directly and resolved
- **WHEN** `ssd_define_scenario(..., min_pmix = c("default", "strict"))` is called
- **THEN** the object SHALL store those names verbatim as `fit$min_pmix` and SHALL materialise the function each name resolves to, keyed by name

#### Scenario: min_pmix list of functions derives names
- **WHEN** `ssd_define_scenario(..., min_pmix = list(ssdtools::ssd_min_pmix))` (unnamed) or `list(strict = ssdtools::ssd_min_pmix)` (named) is called
- **THEN** the object SHALL store the derived element name(s) (e.g. `"ssd_min_pmix"`) or the list names (e.g. `"strict"`) respectively, validate each provided function before taking its name, and materialise the functions keyed by those names

### Requirement: Configurable, validated partition_by argument with a complementary bundle entry point
`ssd_define_scenario()` SHALL accept an optional `partition_by` argument and a complementary optional `bundle` argument. Each, when supplied, is a named list keyed by task step (`sample`/`fit`/`hc`; `task-list-loop-baseline`, #80, with the `data` truncation folded into `fit`) and MAY be partial (cover a subset of steps). A `partition_by` entry SHALL be a character vector of **path-axis** names; a `bundle` entry SHALL be a character vector of **inner-axis** names; in both cases the vector SHALL be unique, free of missing values, and a subset of that step's vocabulary (`task_axes(step)`). For each step **at most one** of `partition_by`/`bundle` SHALL name it; a step named in **both** SHALL abort. A step named in **neither** SHALL use its documented default. The constructor SHALL normalize the two into a single stored `partition_by` — a complete `sample`/`fit`/`hc` **path** list: a `bundle` entry becomes its path complement `setdiff(task_axes(step), bundle[[step]])`, and defaults fill unnamed steps. The constructor SHALL abort, in the context of the user-facing function, with an informative error naming the offending step and axis when any condition is violated.

#### Scenario: Defaults populated when absent
- **WHEN** `ssd_define_scenario()` is called without `partition_by`
- **THEN** the object's `partition_by` SHALL be the documented defaults: `sample = c("dataset", "sim", "replace")`, `fit = c("dataset", "sim", "nrow", "rescale")`, `hc = c("dataset", "sim")`

#### Scenario: Valid override accepted
- **WHEN** `ssd_define_scenario(..., partition_by = list(sample = c("dataset", "sim", "replace"), fit = c("dataset", "sim", "nrow"), hc = c("dataset", "sim", "rescale")))` is called
- **THEN** the object SHALL store that `partition_by` verbatim with no error

#### Scenario: Partial spec defaults the unnamed steps
- **WHEN** `partition_by` (or `bundle`) is supplied covering only some of the `sample`/`fit`/`hc` steps
- **THEN** the steps named in neither argument SHALL use their documented defaults, and the stored `partition_by` SHALL be the complete three-step path list

#### Scenario: bundle accepted as the inner-axis complement
- **WHEN** `ssd_define_scenario(..., bundle = list(fit = c("computable", "at_boundary_ok", "min_pmix", "range_shape1", "range_shape2")))` is called
- **THEN** the stored `fit` `partition_by` SHALL be `setdiff(task_axes("fit"), bundle$fit)` = `c("dataset", "sim", "replace", "nrow", "rescale")`

#### Scenario: partition_by and bundle mixed across steps
- **WHEN** `partition_by` names the `sample` step and `bundle` names the `fit` step (with `hc` omitted)
- **THEN** the constructor SHALL accept it: `sample` uses the given path axes, `fit` uses the `bundle` complement, and `hc` uses its default

#### Scenario: A step named in both partition_by and bundle is rejected
- **WHEN** the same step (e.g. `fit`) appears in both the `partition_by` and the `bundle` argument
- **THEN** the constructor SHALL abort with an informative error naming that step

#### Scenario: Unknown axis rejected
- **WHEN** a step's `partition_by` entry names an axis outside that step's vocabulary (e.g. `nboot` under `sample`, or a typo)
- **THEN** the constructor SHALL abort with an informative error naming the offending step and axis

#### Scenario: nrow rejected only for the sample step
- **WHEN** the `sample` step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL abort with an informative error stating that the `sample` step is the shared draw and carries no `nrow` axis (every `nrow` truncates the same draw inside the `fit` step, §5)

#### Scenario: nrow accepted as a path axis for fit/hc
- **WHEN** the `fit` (or `hc`) step's `partition_by` entry includes `"nrow"`
- **THEN** the constructor SHALL accept it, because the `fit` step truncates its parent sample (`head`, RNG-free) so `nrow` is a genuine `fit` cross-join axis (#80, data folded into fit)

#### Scenario: Duplicate or missing axis names rejected
- **WHEN** a step's `partition_by` entry contains duplicate names or `NA`
- **THEN** the constructor SHALL abort with an informative error

### Requirement: Path-axis vs inner-axis split
The package SHALL define, for each step, the inner (Parquet-column) axes as that step's axis vocabulary (`task_axes()`, #80) minus its `partition_by` path axes, and SHALL expose this split to downstream task-table and shard construction. The path axes determine the shard count for a step (`Π |path axis|`) and the **Hive shard path** (`path_key()` over the chosen path axes — distinct from the `<step>_id` task-identity key, which #80 keys over *all* axes and which `partition_by` does not change); the inner axes are carried as columns within each shard.

#### Scenario: Inner axes are the complement of path axes
- **WHEN** the fit step's vocabulary is queried for a scenario whose `fit` path axes are `c("dataset", "sim", "nrow", "rescale")`
- **THEN** the inner axes SHALL be the remaining fit axes (`replace`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`)

#### Scenario: All-axes-in-path yields no inner axes
- **WHEN** a step's `partition_by` lists every axis in that step's vocabulary
- **THEN** the inner-axis set for that step SHALL be empty (one task per shard — the shard path then equals the `<step>_id` task identity, the only case where they coincide)

#### Scenario: Per-step vocabularies match #80's task_axes()
- **WHEN** the axis vocabulary is queried per step
- **THEN** it SHALL equal `task_axes(step)`: `sample` = `dataset`, `sim`, `replace`; `fit` adds `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `nboot`, `ci_method`, `parametric` (`ci` and `est_method` are hc simulation settings, not axes — see *"ci is a scalar flag selecting bootstrap confidence intervals"* and *"est_method is an hc simulation setting computed from shared samples"*)

#### Scenario: Steps partition independently — no cross-step constraint
- **WHEN** a step's path axes are a valid subset of its own vocabulary but differ arbitrarily from its parent step's path axes (e.g. finer or coarser on a shared axis)
- **THEN** the constructor SHALL accept them without any parent-consistency check, since a child shard may span several parent shards (an m:n relationship resolved at the read layer, not by restricting `partition_by`); the `<parent>_id` foreign key remains well-defined regardless

### Requirement: partition_by and bundle rendered by print method
`print.ssdsims_scenario()` SHALL render, per step, **both** the `partition_by` (across-shards) path axes and the `bundle` (within-shard) inner axes, regardless of which was supplied, alongside the other declarative fields.

#### Scenario: Print shows both path and bundle axes
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the output SHALL show, for the `sample`, `fit`, and `hc` steps, both the path axes (`partition_by`) and the inner axes (`bundle`)

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

### Requirement: ci is a scalar flag selecting bootstrap confidence intervals
`ssd_define_scenario()` SHALL accept `ci` as a scalar logical flag (a single non-`NA` `TRUE`/`FALSE`; default `FALSE`), validated with `chk::chk_flag`, stored at `scenario$hc$ci`, and passed to `ssdtools::ssd_hc()`. `ci` SHALL NOT be a grid axis and SHALL NOT enter the `hc` task identity (`task_axes("hc")`) or the per-task RNG primer: the point estimate `est` is invariant to `ci` (computed analytically from the fit, independent of the bootstrap and RNG), so a single `ci = TRUE` run is a superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is scenario-wide and either/or — `ci = FALSE` for cheap, bootstrap-free point estimates, or `ci = TRUE` for estimates plus confidence intervals. When `ci = FALSE`, supplying any bootstrap-only scenario option (`nboot`, `ci_method`, or `parametric`) SHALL abort in the user-facing frame, directing the user to set `ci = TRUE` or omit the option. `print.ssdsims_scenario()` SHALL render `ci` among the hc scenario options.

#### Scenario: ci defaults to FALSE and is a flag
- **WHEN** `ssd_define_scenario()` is called without `ci`
- **THEN** `scenario$hc$ci` SHALL be the scalar `FALSE`

#### Scenario: A vector ci is rejected
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (or any non-flag `ci`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame, because `ci` is a scalar flag

#### Scenario: Bootstrap-only scenario options rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` (or with `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort with an informative error stating that bootstrap-only scenario options are not allowed when `ci = FALSE`, and directing the user to set `ci = TRUE` or omit the option(s)

#### Scenario: ci = TRUE retains the bootstrap axes
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap axes SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × ci_method × parametric` (with `est_method` summarised within each task, not fanned out)

#### Scenario: ci does not enter the hc task identity
- **WHEN** the `hc` axis vocabulary (`task_axes("hc")`) is queried
- **THEN** it SHALL NOT contain `"ci"`, so `ci` is neither a path axis nor an inner axis and does not change the per-task primer; it is applied uniformly to every hc task

### Requirement: est_method is an hc simulation setting computed from shared samples
`ssd_define_scenario()` SHALL accept `est_method` as an hc-level **simulation
setting** (default `"multi"`), validated as a non-`NA`, unique character vector
that is a subset of `ssdtools::ssd_est_methods()`, stored at
`scenario$hc$est_method`, and passed to the hc step. `est_method` SHALL NOT be a
cross-join axis: it SHALL be absent from `task_axes("hc")`, SHALL NOT enter the
`hc` task identity or the per-task RNG primer, and SHALL NOT be an accepted
`partition_by`/`bundle` axis for the `hc` step. A vector `est_method` SHALL NOT
multiply hc tasks; instead every requested method SHALL be summarised from the
**single** bootstrap sample set of each hc task (each method appears as a row in
that task's `hc` tibble). The role mirrors `proportion`: a within-result
dimension consumed inside one bootstrap, not a task multiplier.

#### Scenario: est_method defaults to multi and is stored as a setting
- **WHEN** `ssd_define_scenario()` is called without `est_method`
- **THEN** `scenario$hc$est_method` SHALL be `"multi"`, and `task_axes("hc")` SHALL NOT contain `"est_method"`

#### Scenario: A vector est_method does not multiply hc tasks
- **WHEN** `ssd_define_scenario(..., ci = TRUE, est_method = c("arithmetic", "geometric", "multi"))` is run
- **THEN** the hc task count SHALL NOT be multiplied by the number of `est_method` values, and each hc task's `hc` tibble SHALL contain one row per requested `est_method`, all derived from that task's single bootstrap sample set

#### Scenario: est_method is rejected as an hc partition axis
- **WHEN** `ssd_define_scenario(..., partition_by = list(hc = "est_method"))` (or the equivalent `bundle`) is called
- **THEN** the constructor SHALL abort, because `"est_method"` is not in the `hc` step's axis vocabulary

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **structural cross-join axes** (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`); (3) the **non-`ci`-gated simulation settings** — scenario options that are valid and meaningful even when `ci = FALSE`: `dists` (fit-level), then `est_method` and `proportion` (hc-level, shaping the analytical point estimate); (4) `ci`, then the scenario options it **gates** — the bootstrap **cross-join axes** `nboot`/`ci_method`/`parametric` (which `ci = FALSE` rejects) and the `samples` setting (which only retains bootstrap draws); (5) the **layout and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any scenario option absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — fanning out within the task's output (`est_method`, `proportion`) or applied uniformly (`ci`, `dists`, `samples`). `dists` is the **fit**-level setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `est_method`/`proportion`/`ci`/`samples` are **hc**-level. `est_method` and `proportion` SHALL precede `ci` (a `ci = FALSE` scenario still selects an `est_method` and `proportion` for its analytical estimate); the bootstrap-only scenario options `nboot`/`ci_method`/`parametric` and `samples` SHALL follow `ci`. Storage SHALL remain step-based: `dists` is stored at `scenario$fit$dists` and the hc scenario options at `scenario$hc` in signature order (`est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, `samples`). `print.ssdsims_scenario()` SHALL render each grid in that stored order (settings flagged) and render `dists` among the fit scenario options, marked as a setting rather than an axis.

#### Scenario: non-ci-gated settings precede ci; gated scenario options follow it
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `dists`, `est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, and `samples` SHALL appear adjacent to one another in that order, after the last structural axis (`range_shape2`) and before the layout arguments (`partition_by`, `bundle`, `upload`), so the non-`ci`-gated settings precede `ci` and the scenario options it gates follow it

#### Scenario: dists is a simulation setting, not an axis
- **WHEN** the fit-step axis vocabulary (`task_axes("fit")`) is queried
- **THEN** it SHALL NOT contain `"dists"`, so `dists` is neither a path axis nor an inner axis and does not enter the per-task primer; it is applied uniformly to every fit task and stored at `scenario$fit$dists`

#### Scenario: Print groups the hc scenario options by ci-gating
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render `est_method`, `proportion`, `ci`, then the bootstrap axes (`nboot`, `ci_method`, `parametric`), then `samples`, with the non-axis scenario options flagged as settings; and the fit grid SHALL render `dists` marked as a setting

### Requirement: samples retains the bootstrap draws (output only)
`ssd_define_scenario()` SHALL accept a scalar logical `samples` argument (default `FALSE`), validated as a flag (a single non-`NA` `TRUE`/`FALSE`), stored at `scenario$hc$samples`, and passed to `ssdtools::ssd_hc()` so that `samples = TRUE` retains the per-row bootstrap draws in the hc `samples` list-column. `samples` SHALL NOT be a grid axis and SHALL NOT enter the task identity (`task_axes("hc")`) or the per-task RNG primer: it does not change the estimates, so changing it SHALL yield byte-identical `est`/`lcl`/`ucl` while re-running the hc step to populate (or empty) the `samples` column. `print.ssdsims_scenario()` SHALL render `samples` among the hc scenario options.

#### Scenario: samples defaults to FALSE and is stored
- **WHEN** `ssd_define_scenario()` is called without `samples`
- **THEN** `scenario$hc$samples` SHALL be `FALSE`

#### Scenario: samples = TRUE retains draws without changing estimates
- **WHEN** a scenario is run with `samples = TRUE` versus `FALSE` (same seed, `ci = TRUE`)
- **THEN** the hc estimates SHALL be byte-identical, and the `samples` list-column SHALL be populated only when `samples = TRUE`

#### Scenario: samples must be a flag
- **WHEN** `ssd_define_scenario(..., samples = c(TRUE, FALSE))` (or any non-flag) is called
- **THEN** the constructor SHALL abort in the user-facing frame

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
