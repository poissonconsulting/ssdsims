## MODIFIED Requirements

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

### Requirement: Derive the hc task table from a scenario
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (`nboot`, `ci_method`, `parametric`). The scenario's scalar `ci` flag SHALL be applied uniformly to every hc task from the scenario (read by the runner) and SHALL NOT be emitted as a task-row column — it is not a cross-join axis and is absent from `task_axes("hc")`. `est_method` SHALL likewise NOT be a cross-join axis: it is an hc simulation setting (a within-task dimension), absent from `task_axes("hc")`, and every requested `est_method` is summarised from the single bootstrap sample set of its hc task rather than fanning out into separate tasks. When `ci = FALSE`, the bootstrap-only scenario options (`nboot`, `ci_method`, `parametric`) SHALL be canonically `NA` on every hc row (so they do not enter task identity for a no-bootstrap estimate), leaving **no** cross-join axis — exactly one hc row per fit task; this canonicalisation is keyed off the scenario's scalar `ci`, not an emitted `ci` column. When `ci = TRUE`, the grid SHALL fan out over `nboot × ci_method × parametric`.

#### Scenario: hc tasks cross fit identity with the hc grid
- **WHEN** the hc task table is derived from a fit task table of `K` rows and an `hc` argument grid of `H` rows (over `nboot × ci_method × parametric`)
- **THEN** the hc task table SHALL have `K * H` rows, each carrying its parent fit-task identity columns and the hc-grid argument columns, SHALL NOT be multiplied by the number of `est_method` values, and SHALL NOT carry a `ci` column

#### Scenario: ci = FALSE yields one hc row per fit task
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = FALSE` and multiple `est_method` values
- **THEN** the expansion SHALL produce exactly one hc row per fit task with `nboot`/`ci_method`/`parametric` set to `NA`, SHALL NOT fan out across `est_method` (all requested methods are summarised within that single task), and SHALL NOT emit a `ci` column

#### Scenario: ci = TRUE fans out over the bootstrap axes
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = TRUE` and multiple `nboot`/`ci_method`/`parametric` values
- **THEN** the expansion SHALL fan out over `nboot × ci_method × parametric`, and neither `ci` nor `est_method` SHALL appear among the per-task identity axes (`task_axes("hc")`) or as an emitted `ci` column

### Requirement: Baseline runner executes the task tables in order
The package SHALL provide a baseline runner that executes the task tables in dependency order — sample, then fit, then hc — using `purrr::pmap()` loops over the derived tables (it SHALL NOT expand the tasks itself), looking up each task's parent result by the parent step's id foreign key, threading each step's output into the next, and returning the collected results. The `sample` step SHALL draw the effective draw size resolved from `scenario$nrow_max` and the dataset; the `fit` step SHALL truncate its parent sample (`head(sample, nrow)`) before fitting; the `hc` step SHALL apply the scenario's scalar `ci` read from the scenario (not a row column). The runner SHALL operate in-process with no `targets` dependency, no shard grouping, and no Parquet I/O.

#### Scenario: Runner threads sample → fit → hc
- **WHEN** the baseline runner is called on a scenario
- **THEN** it SHALL draw the sample tasks at the `nrow_max`-derived draw size, fit each fit task against `head(sample, nrow)` of its parent draw (resolved by `sample_id`), estimate the hc tasks against the fits (resolved by `fit_id`) applying the scenario's scalar `ci`, and SHALL return the collected per-step results

#### Scenario: Runner has no targets or shard machinery
- **WHEN** the baseline runner executes
- **THEN** it SHALL NOT load `targets`, SHALL NOT group rows into shards or apply `partition_by`, and SHALL NOT write Parquet files
