## REMOVED Requirements

### Requirement: Derive the hc task table from a scenario honouring the ci = FALSE collapse
**Reason**: The `ci = FALSE` collapse existed because `ci` was a grid/task axis (`c(FALSE, TRUE)`), so a mixed grid had to stop the `ci = FALSE` portion from fanning out across the bootstrap-only scenario options. With `ci` demoted to a scalar hc flag (removed from `task_axes("hc")`), there is no `ci` axis and no mixed grid to collapse. The hc-table derivation is restated, without the collapse, in *"Derive the hc task table from a scenario"*.
**Migration**: `hc_grid_tbl()` drops its `any(ci == FALSE)` / `any(ci == TRUE)` `bind_rows` branching for a single grid keyed by the scalar `scenario$hc$ci`; the `NA`-canonicalisation of bootstrap-only scenario options is retained only for the `ci = FALSE` case so `task_primer()` stays well-defined.

## ADDED Requirements

### Requirement: Derive the hc task table from a scenario
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (`nboot`, `est_method`, `ci_method`, `parametric`). The scenario's scalar `ci` flag SHALL be applied uniformly to every hc row — it is not a cross-join axis and is absent from `task_axes("hc")`. When `ci = FALSE`, the bootstrap-only scenario options (`nboot`, `ci_method`, `parametric`) SHALL be canonically `NA` on every hc row (so they do not enter task identity for a no-bootstrap estimate), leaving `est_method` as the only fan-out axis; when `ci = TRUE`, the grid SHALL fan out over `nboot × est_method × ci_method × parametric`.

#### Scenario: hc tasks cross fit identity with the hc grid
- **WHEN** the hc task table is derived from a fit task table of `K` rows and an `hc` argument grid of `H` rows
- **THEN** the hc task table SHALL have `K * H` rows, each carrying its parent fit-task identity columns and the hc-grid argument columns

#### Scenario: ci = FALSE leaves bootstrap-only scenario options NA
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = FALSE` and multiple `est_method` values
- **THEN** the expansion SHALL produce one row per `est_method` with `nboot`/`ci_method`/`parametric` set to `NA`, rather than fanning out across those bootstrap-only scenario options

#### Scenario: ci = TRUE fans out over the bootstrap axes
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = TRUE` and multiple `nboot`/`ci_method`/`parametric` values
- **THEN** the expansion SHALL fan out over `nboot × est_method × ci_method × parametric`, and `ci` SHALL NOT appear among the per-task identity axes (`task_axes("hc")`)
