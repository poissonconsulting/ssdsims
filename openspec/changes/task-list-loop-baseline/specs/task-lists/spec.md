## ADDED Requirements

### Requirement: Derive the data task table from a scenario
The package SHALL derive a `data` task table from an `ssdsims_scenario` containing one row per cell of the cross-join of the scenario's dataset names, replicate index (`1:nsim`), and `replace` values. Each row SHALL carry one column per cross-join axis (`dataset`, `sim`, `replace`) and SHALL additionally carry `nrow` as an ordinary (non-identity) column. The derivation SHALL perform no RNG draws and SHALL NOT add any `seed`, `primer`, or `stream` columns.

#### Scenario: One row per (dataset, sim, replace) cell
- **WHEN** the data task table is derived from a scenario with `D` dataset names, `nsim = N`, and `R` distinct `replace` values
- **THEN** the table SHALL have `D * N * R` rows, one per cross-join cell, each with the `dataset`, `sim`, and `replace` columns populated

#### Scenario: nrow is carried but is not an identity axis
- **WHEN** a scenario specifies multiple `nrow` values
- **THEN** the data task table SHALL NOT multiply its row count by the number of `nrow` values; `nrow` SHALL be present as an ordinary column, not a cross-join axis

#### Scenario: Derivation is RNG-free
- **WHEN** the data task table is derived
- **THEN** no random numbers SHALL be drawn, `.Random.seed` SHALL be unchanged, and the table SHALL contain no `seed`/`primer`/`stream` columns

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

### Requirement: Baseline runner executes the three task tables in order
The package SHALL provide a baseline runner that executes the three task tables in dependency order — data, then fit, then hc — using `purrr::pmap()` loops, threading each step's output into the next, and returning the collected results. The runner SHALL operate in-process with no `targets` dependency, no shard grouping, and no Parquet I/O.

#### Scenario: Runner threads data → fit → hc
- **WHEN** the baseline runner is called on a scenario
- **THEN** it SHALL execute the data tasks, then the fit tasks against those outputs, then the hc tasks against the fit outputs, and SHALL return the collected per-step results

#### Scenario: Runner has no targets or shard machinery
- **WHEN** the baseline runner executes
- **THEN** it SHALL NOT load `targets`, SHALL NOT group rows into shards or apply `partition_by`, and SHALL NOT write Parquet files
