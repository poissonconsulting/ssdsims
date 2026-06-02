## ADDED Requirements

### Requirement: Derive the sample task table from a scenario
The package SHALL derive a `sample` task table from an `ssdsims_scenario` containing one row per cell of the cross-join of the scenario's dataset names, replicate index (`1:nsim`), and `replace` values — the single random draw that every `nrow` value sub-truncates (`TARGETS-DESIGN.md` §5). Each row SHALL carry one column per cross-join axis (`dataset`, `sim`, `replace`) and SHALL additionally carry `n_max` (the draw size, `max(nrow)`) as an ordinary (non-identity) column. `nrow` SHALL NOT be a sample axis. The derivation SHALL perform no RNG draws and SHALL NOT add any `seed`, `primer`, or `stream` columns.

#### Scenario: One row per (dataset, sim, replace) cell
- **WHEN** the sample task table is derived from a scenario with `D` dataset names, `nsim = N`, and `R` distinct `replace` values
- **THEN** the table SHALL have `D * N * R` rows, one per cross-join cell, each with the `dataset`, `sim`, and `replace` columns populated

#### Scenario: nrow does not multiply the draw
- **WHEN** a scenario specifies multiple `nrow` values
- **THEN** the sample task table SHALL NOT multiply its row count by the number of `nrow` values; the draw size SHALL be carried as the ordinary `n_max` column, not a cross-join axis

#### Scenario: Derivation is RNG-free
- **WHEN** the sample task table is derived
- **THEN** no random numbers SHALL be drawn, `.Random.seed` SHALL be unchanged, and the table SHALL contain no `seed`/`primer`/`stream` columns

### Requirement: Derive the data task table from a scenario
The package SHALL derive a `data` task table by crossing each sample-task identity (`dataset`, `sim`, `replace`) with the scenario's `nrow` values; each `data` task is the `head(sample, nrow)` truncation of its parent sample. Because the truncation is RNG-free, `nrow` SHALL be an ordinary cross-join axis of this step (one truncation per size) without duplicating the shared draw.

#### Scenario: Data tasks cross sample identity with nrow
- **WHEN** the data task table is derived from a sample task table of `S` rows and a scenario with `|nrow|` sample sizes
- **THEN** the data task table SHALL have `S * |nrow|` rows, each carrying its parent sample-task identity columns and the `nrow` column

### Requirement: Derive the fit task table from a scenario
The package SHALL derive a `fit` task table by crossing each data-task identity with each row of the scenario's `fit` argument grid (e.g. `rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`). Each row SHALL carry the columns identifying its parent data task plus one column per fit-grid axis. `min_pmix` SHALL be referenced by name, not by function value.

#### Scenario: Fit tasks cross data identity with the fit grid
- **WHEN** the fit task table is derived from a data task table of `M` rows and a `fit` argument grid of `F` rows
- **THEN** the fit task table SHALL have `M * F` rows, each carrying its parent data-task identity columns and the fit-grid argument columns

#### Scenario: min_pmix referenced by name
- **WHEN** a fit task references a `min_pmix` entry
- **THEN** the fit task table SHALL store the `min_pmix` name (not the function value)

### Requirement: Derive the hc task table from a scenario honouring the ci = FALSE collapse
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (e.g. `nboot`, `est_method`, `ci_method`, `parametric`). The expansion SHALL honour the construction-time `ci = FALSE` collapse already recorded on the scenario (`TARGETS-DESIGN.md` §1.2): rows where `ci = FALSE` SHALL NOT be multiplied across the bootstrap-only knobs (`nboot`, `ci_method`, `parametric`).

#### Scenario: hc tasks cross fit identity with the hc grid
- **WHEN** the hc task table is derived from a fit task table of `K` rows and an `hc` argument grid of `H` rows
- **THEN** the hc task table SHALL have `K * H` rows, each carrying its parent fit-task identity columns and the hc-grid argument columns

#### Scenario: ci = FALSE collapses the bootstrap-only knobs
- **WHEN** the hc grid contains `ci = FALSE` together with multiple `nboot` / `ci_method` / `parametric` values
- **THEN** the `ci = FALSE` portion of the expansion SHALL collapse to a single row over those bootstrap-only knobs, matching the §1.2 reduced fan-out, rather than the full cross-join

### Requirement: Tasks carry an explicit id and parent foreign key
Each task row SHALL carry a path-style `<step>_id` primary key built from the step's cross-join axes (the Hive partition path, `TARGETS-DESIGN.md` §5/§6), and every non-root step SHALL additionally carry its parent step's id as a foreign key column. Dependencies between steps SHALL therefore be encoded explicitly as a single joinable foreign-key column (`sample_id` on data tasks, `data_id` on fit tasks, `fit_id` on hc tasks), not only as the carried identity columns.

#### Scenario: Each table exposes a primary key and parent foreign key
- **WHEN** the sample, data, fit, or hc task table is derived
- **THEN** each row SHALL carry a unique path-style `<step>_id`, and each non-root table SHALL carry its parent's id as a foreign key whose every value matches a primary key in the parent table

### Requirement: Task tables are a printable S3 class
Each derived task table SHALL be an `ssdsims_tasks` S3 object: a classed tibble that records its step (`sample`, `data`, `fit`, or `hc`) and behaves as an ordinary tibble for data operations. The package SHALL provide a `print.ssdsims_tasks()` method that renders the step name, the cross-join axes, the number of tasks (rows), and a compact preview of the rows.

#### Scenario: A derived task table carries the ssdsims_tasks class
- **WHEN** any of the sample, data, fit, or hc task tables is derived from a scenario
- **THEN** the returned object SHALL inherit from `ssdsims_tasks`, SHALL record its step, and SHALL still support ordinary tibble/data-frame operations

#### Scenario: Printing a task table is informative
- **WHEN** an `ssdsims_tasks` object is printed
- **THEN** the output SHALL identify the step, list the cross-join axes, report the task (row) count, and show a compact preview of the rows

### Requirement: A compound expansion bundles the four task tables
The package SHALL provide a single compound expansion function (`ssd_scenario_tasks()`) that derives all four task tables from a scenario and returns them as one `ssdsims_task_set` S3 object exposing the `sample`, `data`, `fit`, and `hc` tables. This is the canonical expansion entry point (`TARGETS-DESIGN.md` §1/§2); the per-step derivations remain available for callers that need a single table. The package SHALL provide a `print.ssdsims_task_set()` method summarising the per-step task counts.

#### Scenario: Compound expansion returns all four tables
- **WHEN** the compound expansion is called on a scenario
- **THEN** it SHALL return an `ssdsims_task_set` exposing the `sample`, `data`, `fit`, and `hc` tables, each an `ssdsims_tasks` object identical to the one the corresponding per-step derivation would return

#### Scenario: Printing a task set is informative
- **WHEN** an `ssdsims_task_set` object is printed
- **THEN** the output SHALL identify each step and report its task (row) count

### Requirement: Baseline runner executes the four task tables in order
The package SHALL provide a baseline runner that executes the four task tables in dependency order — sample, then data, then fit, then hc — using `purrr::pmap()` loops over the derived tables (it SHALL NOT expand the tasks itself), looking up each task's parent result by the parent step's id foreign key, threading each step's output into the next, and returning the collected results. The runner SHALL operate in-process with no `targets` dependency, no shard grouping, and no Parquet I/O.

#### Scenario: Runner threads sample → data → fit → hc
- **WHEN** the baseline runner is called on a scenario
- **THEN** it SHALL draw the sample tasks, truncate the data tasks against those draws, fit the fit tasks against the truncations, estimate the hc tasks against the fits — resolving each parent by its id foreign key — and SHALL return the collected per-step results

#### Scenario: Runner has no targets or shard machinery
- **WHEN** the baseline runner executes
- **THEN** it SHALL NOT load `targets`, SHALL NOT group rows into shards or apply `partition_by`, and SHALL NOT write Parquet files
