# task-lists Specification

## Purpose

Derive the three per-step task tables (`sample`, `fit`, `hc`) from an `ssdsims_scenario` — the flat per-task data shape the rest of the targets-based pipeline builds on (`TARGETS-DESIGN.md` §1–§2, §5). Keeping the random draw (`sample`) its own step, keyed only by `(dataset, sim, replace)`, keeps the §5 sub-truncation property structural (one draw shared across every `nrow`) while letting `nrow` be a clean `fit` cross-join axis: the `fit` step truncates its parent sample inline (`head(sample, nrow)`, RNG-free) before fitting, so no separate `data` step or shard is needed. Each table carries a path-style `<step>_id` primary key and its parent's id as a foreign key, is a printable `ssdsims_tasks` classed tibble, and is bundled by the canonical `ssd_scenario_tasks()` expansion. A `purrr::pmap()` baseline runner executes the tables in dependency order with no RNG seeding, no `targets`, no shards, and no Parquet I/O.

## Requirements

### Requirement: Derive the sample task table from a scenario
The package SHALL derive a `sample` task table from an `ssdsims_scenario` containing one row per cell of the cross-join of the scenario's dataset names, replicate index (`1:nsim`), and `replace` values — the single random draw that every `nrow` value sub-truncates (`TARGETS-DESIGN.md` §5). Each row SHALL carry one column per cross-join axis (`dataset`, `sim`, `replace`) and SHALL carry **no** non-identity columns: the draw size is the scenario's `nrow_max` setting (resolved by the runner to `min(nrow_max, nrow(data))` for `replace = FALSE` and `nrow_max` for `replace = TRUE`), not a `n_max` row column. `nrow` SHALL NOT be a sample axis. The derivation SHALL perform no RNG draws and SHALL NOT add any `seed`, `primer`, or `stream` columns.

#### Scenario: One row per (dataset, sim, replace) cell
- **WHEN** the sample task table is derived from a scenario with `D` dataset names, `nsim = N`, and `R` distinct `replace` values
- **THEN** the table SHALL have `D * N * R` rows, one per cross-join cell, each with the `dataset`, `sim`, and `replace` columns populated

#### Scenario: nrow does not multiply the draw and the draw size is not a row column
- **WHEN** a scenario specifies multiple `nrow` values
- **THEN** the sample task table SHALL NOT multiply its row count by the number of `nrow` values, and SHALL NOT carry the draw size as a row column (`n_max`); the draw size is determined at run time from the scenario's `nrow_max` setting and the dataset

#### Scenario: Derivation is RNG-free
- **WHEN** the sample task table is derived
- **THEN** no random numbers SHALL be drawn, `.Random.seed` SHALL be unchanged, and the table SHALL contain no `seed`/`primer`/`stream` columns

### Requirement: Derive the fit task table from a scenario
The package SHALL derive a `fit` task table by crossing each **sample-task** identity (`dataset`, `sim`, `replace`) with the scenario's `nrow` values and with each row of the scenario's `fit` argument grid (e.g. `rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`). Each row SHALL carry the columns identifying its parent sample task, the `nrow` column, and one column per fit-grid axis. The `fit` operation SHALL truncate its parent sample inline (`head(sample, nrow)`, RNG-free) before fitting. `min_pmix` SHALL be referenced by name, not by function value.

#### Scenario: Fit tasks cross sample identity with nrow and the fit grid
- **WHEN** the fit task table is derived from a sample task table of `S` rows, a scenario with `|nrow|` sample sizes, and a `fit` argument grid of `F` rows
- **THEN** the fit task table SHALL have `S * |nrow| * F` rows, each carrying its parent sample-task identity columns, the `nrow` column, and the fit-grid argument columns

#### Scenario: Fit truncates its sample inline
- **WHEN** a fit task is run against its parent sample draw
- **THEN** the fit SHALL operate on `head(sample, nrow)` of that draw — no separate `data` task or shard is produced, and the truncation draws no random numbers

#### Scenario: min_pmix referenced by name
- **WHEN** a fit task references a `min_pmix` entry
- **THEN** the fit task table SHALL store the `min_pmix` name (not the function value)

### Requirement: Derive the hc task table from a scenario
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (`nboot`, `ci_method`, `parametric`). The scenario's scalar `ci` flag SHALL be applied uniformly to every hc task from the scenario (read by the runner) and SHALL NOT be emitted as a task-row column — it is not a cross-join axis and is absent from `task_axes("hc")`. `est_method` SHALL likewise NOT be a cross-join axis: it is an hc simulation setting (a within-task dimension), absent from `task_axes("hc")`, and every requested `est_method` is summarised from the single bootstrap sample set of its hc task rather than fanning out into separate tasks. When `ci = FALSE`, the bootstrap-only knobs (`nboot`, `ci_method`, `parametric`) SHALL be canonically `NA` on every hc row (so they do not enter task identity for a no-bootstrap estimate), leaving **no** cross-join axis — exactly one hc row per fit task; this canonicalisation is keyed off the scenario's scalar `ci`, not an emitted `ci` column. When `ci = TRUE`, the grid SHALL fan out over `nboot × ci_method × parametric`.

#### Scenario: hc tasks cross fit identity with the hc grid
- **WHEN** the hc task table is derived from a fit task table of `K` rows and an `hc` argument grid of `H` rows (over `nboot × ci_method × parametric`)
- **THEN** the hc task table SHALL have `K * H` rows, each carrying its parent fit-task identity columns and the hc-grid argument columns, SHALL NOT be multiplied by the number of `est_method` values, and SHALL NOT carry a `ci` column

#### Scenario: ci = FALSE yields one hc row per fit task
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = FALSE` and multiple `est_method` values
- **THEN** the expansion SHALL produce exactly one hc row per fit task with `nboot`/`ci_method`/`parametric` set to `NA`, SHALL NOT fan out across `est_method` (all requested methods are summarised within that single task), and SHALL NOT emit a `ci` column

#### Scenario: ci = TRUE fans out over the bootstrap knobs
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = TRUE` and multiple `nboot`/`ci_method`/`parametric` values
- **THEN** the expansion SHALL fan out over `nboot × ci_method × parametric`, and neither `ci` nor `est_method` SHALL appear among the per-task identity axes (`task_axes("hc")`) or as an emitted `ci` column

### Requirement: Tasks carry an explicit id and parent foreign key
Each task row SHALL carry a path-style `<step>_id` primary key built from the step's cross-join axes (the Hive partition path, `TARGETS-DESIGN.md` §5/§6), and every non-root step SHALL additionally carry its parent step's id as a foreign key column. The step chain is `sample ← fit ← hc`. Dependencies between steps SHALL therefore be encoded explicitly as a single joinable foreign-key column (`sample_id` on fit tasks, `fit_id` on hc tasks), not only as the carried identity columns.

#### Scenario: Each table exposes a primary key and parent foreign key
- **WHEN** the sample, fit, or hc task table is derived
- **THEN** each row SHALL carry a unique path-style `<step>_id`, and each non-root table SHALL carry its parent's id as a foreign key whose every value matches a primary key in the parent table (`sample_id` on fit, `fit_id` on hc)

### Requirement: Task tables are a printable S3 class
Each derived task table SHALL be an `ssdsims_tasks` S3 object: a classed tibble that records its step (`sample`, `fit`, or `hc`) and behaves as an ordinary tibble for data operations. The package SHALL provide a `print.ssdsims_tasks()` method that renders the step name, the cross-join axes, the number of tasks (rows), and a compact preview of the rows.

#### Scenario: A derived task table carries the ssdsims_tasks class
- **WHEN** any of the sample, fit, or hc task tables is derived from a scenario
- **THEN** the returned object SHALL inherit from `ssdsims_tasks`, SHALL record its step, and SHALL still support ordinary tibble/data-frame operations

#### Scenario: Printing a task table is informative
- **WHEN** an `ssdsims_tasks` object is printed
- **THEN** the output SHALL identify the step, list the cross-join axes, report the task (row) count, and show a compact preview of the rows

### Requirement: A compound expansion bundles the task tables
The package SHALL provide a single compound expansion function (`ssd_scenario_tasks()`) that derives all task tables from a scenario and returns them as one `ssdsims_task_set` S3 object exposing the `sample`, `fit`, and `hc` tables. This is the canonical expansion entry point (`TARGETS-DESIGN.md` §1/§2); the per-step derivations remain available for callers that need a single table. The package SHALL provide a `print.ssdsims_task_set()` method summarising the per-step task counts.

#### Scenario: Compound expansion returns the three tables
- **WHEN** the compound expansion is called on a scenario
- **THEN** it SHALL return an `ssdsims_task_set` exposing the `sample`, `fit`, and `hc` tables, each an `ssdsims_tasks` object identical to the one the corresponding per-step derivation would return

#### Scenario: Printing a task set is informative
- **WHEN** an `ssdsims_task_set` object is printed
- **THEN** the output SHALL identify each step and report its task (row) count

### Requirement: Baseline runner executes the task tables in order
The package SHALL provide a baseline runner that executes the task tables in dependency order — sample, then fit, then hc — using `purrr::pmap()` loops over the derived tables (it SHALL NOT expand the tasks itself), looking up each task's parent result by the parent step's id foreign key, threading each step's output into the next, and returning the collected results. The `sample` step SHALL draw the effective draw size resolved from `scenario$nrow_max` and the dataset; the `fit` step SHALL truncate its parent sample (`head(sample, nrow)`) before fitting; the `hc` step SHALL apply the scenario's scalar `ci` read from the scenario (not a row column). The runner SHALL operate in-process with no `targets` dependency, no shard grouping, and no Parquet I/O.

#### Scenario: Runner threads sample → fit → hc
- **WHEN** the baseline runner is called on a scenario
- **THEN** it SHALL draw the sample tasks at the `nrow_max`-derived draw size, fit each fit task against `head(sample, nrow)` of its parent draw (resolved by `sample_id`), estimate the hc tasks against the fits (resolved by `fit_id`) applying the scenario's scalar `ci`, and SHALL return the collected per-step results

#### Scenario: Runner has no targets or shard machinery
- **WHEN** the baseline runner executes
- **THEN** it SHALL NOT load `targets`, SHALL NOT group rows into shards or apply `partition_by`, and SHALL NOT write Parquet files
