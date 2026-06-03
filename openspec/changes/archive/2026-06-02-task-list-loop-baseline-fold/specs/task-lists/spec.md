## REMOVED Requirements

### Requirement: Derive the data task table from a scenario
**Reason**: The standalone RNG-free `head(sample, nrow)` truncation is folded into the `fit` step (`head()` is negligible; a materialised `data` shard merely duplicates a prefix of its `sample` shard, and `fit` is its only consumer).
**Migration**: Drop `ssd_scenario_data_tasks()` and the `data` element of `ssd_scenario_tasks()`. Callers that need the truncated data take `head(sample, nrow)` themselves (which `fit` now does internally). `fit` tasks reference `sample_id` directly and carry `nrow`.

## MODIFIED Requirements

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
The package SHALL provide a baseline runner that executes the task tables in dependency order — sample, then fit, then hc — using `purrr::pmap()` loops over the derived tables (it SHALL NOT expand the tasks itself), looking up each task's parent result by the parent step's id foreign key, threading each step's output into the next, and returning the collected results. The `fit` step SHALL truncate its parent sample (`head(sample, nrow)`) before fitting. The runner SHALL operate in-process with no `targets` dependency, no shard grouping, and no Parquet I/O.

#### Scenario: Runner threads sample → fit → hc
- **WHEN** the baseline runner is called on a scenario
- **THEN** it SHALL draw the sample tasks, fit each fit task against `head(sample, nrow)` of its parent draw (resolved by `sample_id`), estimate the hc tasks against the fits (resolved by `fit_id`), and SHALL return the collected per-step results

#### Scenario: Runner has no targets or shard machinery
- **WHEN** the baseline runner executes
- **THEN** it SHALL NOT load `targets`, SHALL NOT group rows into shards or apply `partition_by`, and SHALL NOT write Parquet files
