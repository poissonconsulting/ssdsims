## REMOVED Requirements

### Requirement: ci = FALSE rejects bootstrap-only knobs
**Reason**: This requirement existed to manage the `ci = c(FALSE, TRUE)` axis (rejecting bootstrap-only knobs when `ci = FALSE` was the *only* value, and steering users to `ci = c(FALSE, TRUE)` to "reduce the scenario's asymmetry"). With `ci` demoted to a scalar flag, that multi-value story no longer exists. The bootstrap-knob rejection itself survives — folded into the new *"ci is a scalar flag selecting bootstrap confidence intervals"* requirement — but its enablement path becomes `ci = TRUE`, not `ci = c(FALSE, TRUE)`.
**Migration**: Replace any `ci = c(FALSE, TRUE)` with a scalar `ci = TRUE` (estimates plus CIs) or `ci = FALSE` (point estimates only); the two cannot be combined in one scenario because a `ci = TRUE` run already contains the `ci = FALSE` estimate byte-identically.

## ADDED Requirements

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
- **THEN** the bootstrap knobs SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × est_method × ci_method × parametric`

#### Scenario: ci does not enter the hc task identity
- **WHEN** the `hc` axis vocabulary (`task_axes("hc")`) is queried
- **THEN** it SHALL NOT contain `"ci"`, so `ci` is neither a path axis nor an inner axis and does not change the per-task primer; it is applied uniformly to every hc task

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role so that arguments of the same kind are contiguous, in this sequence: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **cross-join axes** — the grid knobs that fan out over tasks (`nrow`, `replace`, `dists`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`, `nboot`, `est_method`, `ci_method`, `parametric`); (3) the **simulation settings** — the non-axis knobs consumed within each task (`proportion`, `ci`, `samples`); (4) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any knob absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — fanning out within the task's output (`proportion`) or applied uniformly (`ci`, `samples`). The simulation settings SHALL be contiguous, so `proportion`, `ci`, and `samples` sit together after the last axis (`parametric`) rather than interleaved among the axes as they are today. The stored `scenario$hc` field and `print.ssdsims_scenario()` SHALL list the hc knobs in the same role order (hc axes, then the hc simulation settings `proportion`/`ci`/`samples`).

#### Scenario: Simulation settings are contiguous and follow the axes
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `proportion`, `ci`, and `samples` SHALL appear adjacent to one another, after the last cross-join axis (`parametric`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`)

#### Scenario: Print groups the hc knobs by role
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render the axes first (`nboot`, `est_method`, `ci_method`, `parametric`) and the simulation settings (`proportion`, `ci`, `samples`) together after them

## MODIFIED Requirements

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
- **THEN** it SHALL equal `task_axes(step)`: `sample` = `dataset`, `sim`, `replace`; `fit` adds `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `nboot`, `est_method`, `ci_method`, `parametric` (`ci` is a scalar hc knob, not an axis — see *"ci is a scalar flag selecting bootstrap confidence intervals"*)

#### Scenario: Steps partition independently — no cross-step constraint
- **WHEN** a step's path axes are a valid subset of its own vocabulary but differ arbitrarily from its parent step's path axes (e.g. finer or coarser on a shared axis)
- **THEN** the constructor SHALL accept them without any parent-consistency check, since a child shard may span several parent shards (an m:n relationship resolved at the read layer, not by restricting `partition_by`); the `<parent>_id` foreign key remains well-defined regardless
