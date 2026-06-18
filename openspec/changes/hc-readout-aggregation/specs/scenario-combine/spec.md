# hc-readout-aggregation Specification (delta)

## REMOVED Requirements

### Requirement: Members in a seed group must agree on hc readout settings
**Reason**: Replaced by aggregation — members may now differ in the four non-axis
hc settings (`proportion`, `est_method`, `ci`, `samples`) **and in their fit
`dists` union**, and are reconciled into shared shards rather than rejected.
**Migration**: No caller action; designs that previously aborted with the
"per-overlap aggregation not yet supported" error now run. Only `nrow_max` and
`partition_by` remain uniform-required.

## ADDED Requirements

### Requirement: hc readout settings are aggregated per overlapping cell
The design factory SHALL allow members of a seed group to differ in the four
**non-axis** hc settings (`proportion`, `est_method`, `ci`, `samples`) and SHALL
reconcile them **per shared hc cell, over only the members whose task set contains
that cell**: `proportion` and `est_method` SHALL be **unioned** and `ci` and
`samples` SHALL be reduced with **`any()`**. A cell reached by only one member
SHALL keep that member's demand, so the bootstrap SHALL run only where a
`ci = TRUE` member has tasks. The draw-shaping axes
`nboot`/`ci_method`/`parametric` and `distset` SHALL remain cell axes (in the
primer) and SHALL NOT be aggregated. The aggregation SHALL reuse
`ssdtools::ssd_hc()`'s vectorized `proportion`/`est_method` and
`hc_collapse_est_methods()` with no change to ssdtools, and SHALL preserve
byte-identity: a member's per-task results SHALL equal its standalone-run results.

#### Scenario: Readout-only differences share the hc cell
- **WHEN** two members of a seed group at a coincident hc cell differ only in
  `proportion`/`est_method`/`ci`/`samples`
- **THEN** that cell SHALL be one shard carrying the union of their `proportion`/
  `est_method` and `any()` of `ci`/`samples`, and each member's summary SHALL
  contain exactly its requested readout rows

#### Scenario: Differing hc readouts no longer abort
- **WHEN** `ssd_design_targets(design)` is called for a design whose seed-group
  members differ only in `proportion`/`est_method`/`ci`/`samples`
- **THEN** it SHALL return the target list (no abort), reconciling the readouts by
  aggregation

### Requirement: Differing fit dists unions are reconciled by a design-wide union fit
The design factory SHALL allow members of a seed group to differ in their fit
`dists` union and SHALL reconcile them by fitting the **design-wide union** of the
members' `dists` once per fit cell, each member subsetting via its `distset` hc
axis. Members differing only in `distset` coverage SHALL therefore share every
`sample` and `fit` shard, differing only in their `distset` hc cells. This SHALL
preserve byte-identity — a member's subset hc SHALL equal its standalone fit's hc
(distset-subset-invariance: per-dist fits are independent, so fitting extra dists
does not change a member's subset). Only `nrow_max` and `partition_by` SHALL
remain uniform-required across a seed group.

#### Scenario: distset-coverage members share sample and fit
- **WHEN** two members of a seed group differ only in their `distset` coverage
  (hence in their fit `dists` union), under the same `seed`
- **THEN** they SHALL share every `sample` and `fit` shard (one design-wide union
  fit) and differ only in their `distset` hc cells, and each member's hc results
  SHALL equal its standalone run

### Requirement: ci-mismatched members share via analytical-est routing
The design factory SHALL, when a seed group mixes `ci = FALSE` and `ci = TRUE`
members, compute the hc shards as every `ci = TRUE` member's cells plus the
`ci = FALSE` cells with no overlapping `ci = TRUE` shard at the same
`(fit-id, distset)`. A `ci = FALSE` task's analytical point `est` SHALL be served
by the coincident `ci = TRUE` shard when one exists, and that `est` SHALL equal the
value the member would have computed standalone with `ci = FALSE`. A `ci = TRUE`
member's confidence interval SHALL use its own cell's
`(nboot, ci_method, parametric)` primer.

#### Scenario: Bootstrap only the overlapping cells
- **WHEN** a design unions `ci = FALSE, nsim = 1000` and `ci = TRUE, nsim = 10`
  (otherwise shareable, same `seed`)
- **THEN** the hc shards for the 10 overlapping sims SHALL be computed with
  `ci = TRUE`, the remaining 990 SHALL be computed `ci = FALSE`, and the
  `ci = FALSE` member SHALL read its (analytical, ci-invariant) `est` for the
  overlapping sims from the `ci = TRUE` shards

#### Scenario: Single-scenario hc results are unchanged
- **WHEN** a scenario is run via `ssd_scenario_targets()` (no per-task readout
  overrides)
- **THEN** its `hc` per-task results SHALL be byte-identical to before this change
