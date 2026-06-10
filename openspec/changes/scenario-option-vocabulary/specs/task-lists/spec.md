# task-lists Delta

## MODIFIED Requirements

### Requirement: Derive the hc task table from a scenario
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (`nboot`, `ci_method`, `parametric`). The scenario's scalar `ci` flag SHALL be applied uniformly to every hc row — it is not a scenario axis and is absent from `task_axes("hc")`. `est_method` SHALL likewise NOT be a scenario axis: it is an hc scenario setting (a within-task dimension), absent from `task_axes("hc")`, and every requested `est_method` is summarised from the single bootstrap sample set of its hc task rather than fanning out into separate tasks. When `ci = FALSE`, the bootstrap-only scenario options (`nboot`, `ci_method`, `parametric`) SHALL be canonically `NA` on every hc row (so they do not enter task identity for a no-bootstrap estimate), leaving **no** cross-join axis — exactly one hc row per fit task; when `ci = TRUE`, the grid SHALL fan out over `nboot × ci_method × parametric`.

#### Scenario: hc tasks cross fit identity with the hc grid
- **WHEN** the hc task table is derived from a fit task table of `K` rows and an `hc` argument grid of `H` rows (over `nboot × ci_method × parametric`)
- **THEN** the hc task table SHALL have `K * H` rows, each carrying its parent fit-task identity columns and the hc-grid argument columns, and SHALL NOT be multiplied by the number of `est_method` values

#### Scenario: ci = FALSE yields one hc row per fit task
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = FALSE` and multiple `est_method` values
- **THEN** the expansion SHALL produce exactly one hc row per fit task with `nboot`/`ci_method`/`parametric` set to `NA`, and SHALL NOT fan out across `est_method` (all requested methods are summarised within that single task)

#### Scenario: ci = TRUE fans out over the bootstrap axes
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = TRUE` and multiple `nboot`/`ci_method`/`parametric` values
- **THEN** the expansion SHALL fan out over `nboot × ci_method × parametric`, and neither `ci` nor `est_method` SHALL appear among the per-task identity axes (`task_axes("hc")`)
