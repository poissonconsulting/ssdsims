# scenario-definition Specification

## Purpose

Provide the purely declarative `ssdsims_scenario` object that roots the targets-based pipeline (`TARGETS-DESIGN.md` §1): its constructor `ssd_define_scenario()`, the `ssd_data()` input normaliser, dataset and `min_pmix` name references, field validation, the scalar `ci` flag (with its bootstrap-knob rejection when `ci = FALSE`), and the `print()` method. The object stores only declarative fields — never data frames, RNG state, or function bodies — so a scenario serialises to a compact manifest and the shard set a pipeline expands to is a pure function of the scenario.
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

### Requirement: Configurable, validated partition_by knob with a complementary bundle entry point
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

### Requirement: ci is a scalar flag selecting bootstrap confidence intervals
`ssd_define_scenario()` SHALL accept `ci` as a scalar logical flag (a single non-`NA` `TRUE`/`FALSE`; default `FALSE`), validated with `chk::chk_flag`, stored at `scenario$hc$ci`, and passed to `ssdtools::ssd_hc()`. `ci` SHALL NOT be a grid axis and SHALL NOT enter the `hc` task identity (`task_axes("hc")`) or the per-task RNG primer: the point estimate `est` is invariant to `ci` (computed analytically from the fit, independent of the bootstrap and RNG), so a single `ci = TRUE` run is a superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is scenario-wide and either/or — `ci = FALSE` for cheap, bootstrap-free point estimates, or `ci = TRUE` for estimates plus confidence intervals. When `ci = FALSE`, supplying any bootstrap-only knob (`nboot`, `ci_method`, or `parametric`) SHALL abort in the user-facing frame, directing the user to set `ci = TRUE` or omit the knob. `print.ssdsims_scenario()` SHALL render `ci` among the hc knobs.

#### Scenario: ci defaults to FALSE and is a flag
- **WHEN** `ssd_define_scenario()` is called without `ci`
- **THEN** `scenario$hc$ci` SHALL be the scalar `FALSE`

#### Scenario: A vector ci is rejected
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (or any non-flag `ci`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame, because `ci` is a scalar flag

#### Scenario: Bootstrap knobs rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` (or with `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort with an informative error stating that bootstrap-only knobs are not allowed when `ci = FALSE`, and directing the user to set `ci = TRUE` or omit the knob(s)

#### Scenario: ci = TRUE retains bootstrap knobs
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap knobs SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × ci_method × parametric` (with `est_method` summarised within each task, not fanned out)

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

### Requirement: nrow_max sets the shared sample draw size
`ssd_define_scenario()` SHALL accept `nrow_max` as a scalar whole number
(default a reasonably high value, `1000L`), validated with
`chk::chk_whole_number`, stored on the scenario, and used as the **fixed**
size of the shared `sample` draw — replacing the previously derived
`max(scenario$nrow)`. `nrow_max` is a sample-level **simulation setting**: it
SHALL NOT be a cross-join axis, SHALL NOT enter any `task_axes(step)` or the
per-task RNG primer, and SHALL NOT be carried as a task-row column. The
effective per-dataset draw size SHALL be `min(nrow_max, nrow(data))` for
`replace = FALSE` (so a high `nrow_max` yields the full permutation) and
`nrow_max` for `replace = TRUE`; it is resolved by the runner from `nrow_max`
and the dataset, not stored per task. Because the fixed `nrow_max` draw is the
largest sample any `nrow` can sub-truncate, `nrow_max` is the **universal
ceiling** for `nrow`: each `nrow` value SHALL be a whole number in
`[5, nrow_max]` (5 being the fit floor), validated at construction, and a value
outside that range SHALL abort in the user-facing frame with a message that
**cites `nrow_max`'s value** so a raised ceiling is discoverable. Per-dataset
`replace = FALSE` infeasibility *within* that range — an `nrow` no greater than
`nrow_max` but greater than a dataset's row count, whose permutation draw caps
at `min(nrow_max, nrow(data))` — is **not** an error here; it is governed by the
`replace`-default requirement (a silent per-cell discard in task expansion).
`nrow_max` itself SHALL be a whole number of at least 5. Because the draw size
no longer depends on `max(nrow)`, adding an `nrow` value (within the draw size)
SHALL NOT change the shared draw. `nrow_max` is **not** `ci`-gated: the draw
happens regardless of `ci`.

#### Scenario: nrow_max defaults high and is a whole number
- **WHEN** `ssd_define_scenario()` is called without `nrow_max`
- **THEN** the scenario SHALL store `nrow_max = 1000L`, and a non-whole-number `nrow_max` SHALL abort in the user-facing frame

#### Scenario: nrow exceeding nrow_max aborts, citing nrow_max
- **WHEN** `ssd_define_scenario(..., nrow = 50, nrow_max = 20)` is called (an `nrow` above the fixed draw size, regardless of `replace`)
- **THEN** the constructor SHALL abort in the user-facing frame with a message citing `nrow_max`'s value (`= 20`), because no `nrow` can sub-truncate a draw it exceeds; and `ssd_define_scenario(..., nrow = c(10, 10000), nrow_max = 10000, replace = c(TRUE, FALSE))` SHALL instead succeed, since `nrow_max` admits `nrow = 10000`

#### Scenario: nrow_max is a simulation setting, not an axis
- **WHEN** the `sample` axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` never multiplies tasks, enters a primer, or becomes a task-row column

### Requirement: replace defaults to sampling with replacement
`ssd_define_scenario()` SHALL default `replace` to `TRUE`, so a scenario built
without an explicit `replace` samples with replacement and its shared draw is
`nrow_max` rows per `(dataset, sim, replace)` cell — decoupling the default
`nrow` sweep from each dataset's size. `replace` SHALL remain a structural
cross-join axis (one or two unique, non-missing logical values), and an
explicit `replace = FALSE` (the permutation draw, capped at
`min(nrow_max, nrow(data))`) SHALL behave as specified below.

Because a `replace = FALSE` permutation draw cannot exceed
`min(nrow_max, nrow(data))` for a given dataset, where `replace` includes
`FALSE` any `(dataset, nrow)` combination with
`nrow > min(nrow_max, nrow(data))` SHALL be silently discarded from the
`replace = FALSE` portion of the task grid — the constructor SHALL NOT abort, emit
a warning, or permit a short draw for it. The discard SHALL be per dataset, and
the corresponding `replace = TRUE` cells together with all feasible
`replace = FALSE` cells SHALL be unaffected. The task grid SHALL therefore be
the cross-join of the scenario's axes minus these infeasible `replace = FALSE`
cells, and SHALL remain a deterministic function of the scenario.

An `nrow` exceeding `nrow_max` itself (the scenario's own draw ceiling,
independent of any dataset) is out of the `[5, nrow_max]` range every `nrow`
must satisfy, so it SHALL abort construction in the user-facing frame
regardless of `replace` — with a message citing `nrow_max`'s value — rather than
being discarded (no draw of any `replace` can supply it). A scenario whose grid
is left empty by the `replace = FALSE` discard SHALL likewise abort.

#### Scenario: replace defaults to TRUE
- **WHEN** `ssd_define_scenario()` is called without `replace`
- **THEN** the scenario SHALL store `replace = TRUE`, the derived `sample` task
  table's `replace` values SHALL all be `TRUE`, and an `nrow` above a dataset's
  row count (but within `nrow_max`) SHALL be accepted

#### Scenario: mixed replace discards an nrow infeasible for the FALSE draw
- **WHEN** `ssd_define_scenario(..., nrow = 50L, replace = c(FALSE, TRUE))` is
  called with a dataset of fewer than 50 rows
- **THEN** the constructor SHALL succeed, the `replace = TRUE` cells at
  `nrow = 50L` SHALL be present in the task grid, and the `replace = FALSE`
  cells at `nrow = 50L` SHALL be absent (silently discarded, with no warning)

#### Scenario: the discard is per dataset
- **WHEN** `ssd_define_scenario()` is called with `replace` including `FALSE`,
  several datasets, and an `nrow` exceeding only the smallest dataset's
  effective draw size
- **THEN** only that dataset's `replace = FALSE` cell at that `nrow` SHALL be
  discarded; the other datasets SHALL retain their `replace = FALSE` cells at
  that `nrow`

#### Scenario: an all-infeasible FALSE-only grid aborts
- **WHEN** `ssd_define_scenario()` is called with `replace = FALSE` only and
  every `nrow` exceeds every dataset's effective draw size
- **THEN** the discard SHALL leave no feasible task and the constructor SHALL
  abort in the user-facing frame rather than return an empty scenario

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **structural cross-join axes** (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`); (3) the **non-`ci`-gated simulation settings** — knobs that are valid and meaningful even when `ci = FALSE`: `nrow_max` (sample-level, the shared draw size), `dists` (fit-level), then `est_method` and `proportion` (hc-level, shaping the analytical point estimate); (4) `ci`, then the knobs it **gates** — the bootstrap **cross-join axes** `nboot`/`ci_method`/`parametric` (which `ci = FALSE` rejects) and the `samples` setting (which only retains bootstrap draws); (5) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any knob absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — setting the shared draw size (`nrow_max`), fanning out within the task's output (`est_method`, `proportion`), or applied uniformly (`ci`, `dists`, `samples`). `nrow_max` is the **sample**-level setting (the fixed shared-draw size, absent from `task_axes("sample")`); `dists` is the **fit**-level setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `est_method`/`proportion`/`ci`/`samples` are **hc**-level. `nrow_max`, `dists`, `est_method`, and `proportion` SHALL precede `ci` (none are `ci`-gated — the draw, the fit, and the analytical estimate all happen regardless of `ci`); the bootstrap-only knobs `nboot`/`ci_method`/`parametric` and `samples` SHALL follow `ci`. Storage SHALL remain step-based: `nrow_max` is stored at the sample level, `dists` at `scenario$fit$dists`, and the hc knobs at `scenario$hc` in signature order (`est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, `samples`). `print.ssdsims_scenario()` SHALL render each grid in that stored order (settings flagged), render `nrow_max` among the sample knobs, and render `dists` among the fit knobs, both marked as settings rather than axes.

#### Scenario: non-ci-gated settings precede ci; gated knobs follow it
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `nrow_max`, `dists`, `est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, and `samples` SHALL appear adjacent to one another in that order, after the last structural axis (`range_shape2`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`), so the non-`ci`-gated settings precede `ci` and the knobs it gates follow it

#### Scenario: nrow_max is a sample-level simulation setting
- **WHEN** the sample-step axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` is neither a path axis nor an inner axis and does not enter the per-task primer; it sets the shared draw size and is stored at the sample level

#### Scenario: Print groups the knobs by ci-gating
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render `est_method`, `proportion`, `ci`, then the bootstrap axes (`nboot`, `ci_method`, `parametric`), then `samples`, with the non-axis knobs flagged as settings; and the sample/fit knobs SHALL render `nrow_max` and `dists` marked as settings

### Requirement: samples retains the bootstrap draws (output only)
`ssd_define_scenario()` SHALL accept a scalar logical `samples` argument (default `FALSE`), validated as a flag (a single non-`NA` `TRUE`/`FALSE`), stored at `scenario$hc$samples`, and passed to `ssdtools::ssd_hc()` so that `samples = TRUE` retains the per-row bootstrap draws in the hc `samples` list-column. `samples` SHALL NOT be a grid axis and SHALL NOT enter the task identity (`task_axes("hc")`) or the per-task RNG primer: it does not change the estimates, so changing it SHALL yield byte-identical `est`/`lcl`/`ucl` while re-running the hc step to populate (or empty) the `samples` column. `print.ssdsims_scenario()` SHALL render `samples` among the hc knobs.

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

