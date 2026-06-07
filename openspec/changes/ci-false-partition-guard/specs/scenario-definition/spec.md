## MODIFIED Requirements

### Requirement: ci is a scalar flag selecting bootstrap confidence intervals
`ssd_define_scenario()` SHALL accept `ci` as a scalar logical flag (a single non-`NA` `TRUE`/`FALSE`; default `FALSE`), validated with `chk::chk_flag`, stored at `scenario$hc$ci`, and passed to `ssdtools::ssd_hc()`. `ci` SHALL NOT be a grid axis and SHALL NOT enter the `hc` task identity (`task_axes("hc")`) or the per-task RNG primer: the point estimate `est` is invariant to `ci` (computed analytically from the fit, independent of the bootstrap and RNG), so a single `ci = TRUE` run is a superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is scenario-wide and either/or — `ci = FALSE` for cheap, bootstrap-free point estimates, or `ci = TRUE` for estimates plus confidence intervals. When `ci = FALSE`, supplying any bootstrap-only knob (`nboot`, `ci_method`, or `parametric`) SHALL abort in the user-facing frame, directing the user to set `ci = TRUE` or omit the knob. When `ci = FALSE`, naming a bootstrap-only hc axis (`nboot`, `ci_method`, or `parametric`) as a layout knob — in `partition_by$hc` (a path axis) or `bundle$hc` (an inner axis) — SHALL likewise abort in the user-facing frame with the same direction, because under `ci = FALSE` those axes are canonically `NA` and partitioning or bundling on them would produce a degenerate all-`NA` Hive partition; this carve-out applies in addition to (not instead of) the generic `task_axes("hc")` subset check, and leaves `task_axes("hc")` itself unchanged (the hc shard schema stays fixed-shape regardless of `ci`). When `ci = TRUE`, those axes SHALL be accepted in `partition_by$hc`/`bundle$hc` as before. `print.ssdsims_scenario()` SHALL render `ci` among the hc knobs.

#### Scenario: ci defaults to FALSE and is a flag
- **WHEN** `ssd_define_scenario()` is called without `ci`
- **THEN** `scenario$hc$ci` SHALL be the scalar `FALSE`

#### Scenario: A vector ci is rejected
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (or any non-flag `ci`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame, because `ci` is a scalar flag

#### Scenario: Bootstrap knobs rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` (or with `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort with an informative error stating that bootstrap-only knobs are not allowed when `ci = FALSE`, and directing the user to set `ci = TRUE` or omit the knob(s)

#### Scenario: Bootstrap-only axes rejected in partition_by$hc when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, partition_by = list(hc = c("dataset", "sim", "nboot")))` (or naming `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame with an informative error naming the offending bootstrap-only axis and directing the user to set `ci = TRUE` or drop it, rather than producing a shard partitioned on an all-`NA` axis

#### Scenario: Bootstrap-only axes rejected in bundle$hc when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, bundle = list(hc = c("nboot")))` (or naming `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame with the same informative error, because a bootstrap-only inner axis is as degenerate under `ci = FALSE` as a bootstrap-only path axis

#### Scenario: Non-bootstrap hc axes still accepted in partition_by/bundle when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, partition_by = list(hc = c("dataset", "sim", "est_method")))` is called
- **THEN** the constructor SHALL accept it, because `est_method` is the lone hc fan-out axis under `ci = FALSE` and is not gated by `ci`

#### Scenario: Bootstrap-only axes accepted in partition_by/bundle when ci = TRUE
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), partition_by = list(hc = c("dataset", "sim", "nboot")))` (or via `bundle$hc`) is called
- **THEN** the constructor SHALL accept the bootstrap-only axis, because under `ci = TRUE` those axes carry real values and are genuine hc fan-out axes

#### Scenario: ci = TRUE retains bootstrap knobs
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap knobs SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × est_method × ci_method × parametric`

#### Scenario: ci does not enter the hc task identity
- **WHEN** the `hc` axis vocabulary (`task_axes("hc")`) is queried
- **THEN** it SHALL NOT contain `"ci"`, so `ci` is neither a path axis nor an inner axis and does not change the per-task primer; it is applied uniformly to every hc task
