## MODIFIED Requirements

### Requirement: Derive the hc task table from a scenario
The package SHALL derive an `hc` task table by crossing each fit-task identity with each row of the scenario's `hc` argument grid (`nboot`, `ci_method`, `parametric`) **and with the scenario's declared distribution sets** (`distset`, the set *names* from `scenario$hc$distsets`). `"distset"` SHALL be a member of `task_axes("hc")` so it enters the per-task primer and the partition/path split. When the scenario declares a single (anonymous) set, the `distset` axis SHALL have one value and SHALL NOT multiply the table. The scenario's scalar `ci` flag SHALL be applied uniformly to every hc row — it is not a cross-join axis and is absent from `task_axes("hc")`. `est_method` SHALL likewise NOT be a cross-join axis: it is an hc simulation setting (a within-task dimension), absent from `task_axes("hc")`, and every requested `est_method` is summarised from the single bootstrap sample set of its hc task rather than fanning out into separate tasks. When `ci = FALSE`, the bootstrap-only knobs (`nboot`, `ci_method`, `parametric`) SHALL be canonically `NA` on every hc row (so they do not enter task identity for a no-bootstrap estimate), leaving the `distset` axis as the only fan-out — one hc row per (fit task × distset); when `ci = TRUE`, the grid SHALL fan out over `distset × nboot × ci_method × parametric`. Each hc task row SHALL carry its `distset` name and its parent `fit_id` (the union fit it subsets).

#### Scenario: hc tasks cross fit identity with the hc grid and the distribution sets
- **WHEN** the hc task table is derived from a fit task table of `K` rows, an `hc` argument grid of `H` rows (over `nboot × ci_method × parametric`), and `D` declared distribution sets
- **THEN** the hc task table SHALL have `K * H * D` rows, each carrying its parent fit-task identity columns, the hc-grid argument columns, and its `distset` name, and SHALL NOT be multiplied by the number of `est_method` values

#### Scenario: A single anonymous set does not multiply the table
- **WHEN** the hc task table is derived from a scenario whose `dists` is a bare character vector (one anonymous set)
- **THEN** the `distset` axis SHALL have exactly one value and the table SHALL match the pre-change row count (one hc row per fit task when `ci = FALSE`)

#### Scenario: ci = FALSE yields one hc row per fit task per distset
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = FALSE`, `D` distribution sets, and multiple `est_method` values
- **THEN** the expansion SHALL produce exactly `D` hc rows per fit task (one per `distset`) with `nboot`/`ci_method`/`parametric` set to `NA`, and SHALL NOT fan out across `est_method`

#### Scenario: ci = TRUE fans out over the distset and bootstrap knobs
- **WHEN** the hc task table is derived from a scenario with the scalar `ci = TRUE`, `D` distribution sets, and multiple `nboot`/`ci_method`/`parametric` values
- **THEN** the expansion SHALL fan out over `distset × nboot × ci_method × parametric`, `"distset"` SHALL appear among the per-task identity axes (`task_axes("hc")`), and neither `ci` nor `est_method` SHALL
