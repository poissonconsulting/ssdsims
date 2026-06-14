# scenario-combine Specification (delta)

## ADDED Requirements

### Requirement: A design is a named scenario collection with a consistency contract
The package SHALL provide `ssd_design(...)` returning a classed `ssdsims_design`
named collection of `ssdsims_scenario` objects — the **design** (DoE sense: the
set of conditions to run), the union of regular per-scenario grids into one
possibly-non-regular design. Names SHALL be taken from explicit argument names or
derived from the argument expression (mirroring the `ssd_data()` derivation); each
name is a **scenario name** used only as the `scenario` identity-column value and
the per-scenario summary target-name suffix — never in a shard path, shard target
name, the per-task primer, or any result value. The constructor SHALL accept
**one or more** scenarios (a design of one is valid and uniformly shaped) and
SHALL abort on an empty call. It SHALL validate that every element is an
`ssdsims_scenario` and that names are unique, non-empty, non-`NA`, and safe
(letters, digits, underscore; starting with a letter). It SHALL further enforce a
**name→value consistency contract** across members: the same `dataset` name SHALL
bind identical data, the same `min_pmix` name SHALL bind an identical function, the
same `distset` name SHALL bind identical members, and `partition_by` SHALL be
identical across members — aborting with an informative error naming the offending
binding otherwise. Construction SHALL be RNG-free.

#### Scenario: Names are derived or explicit
- **WHEN** `ssd_design(coarse, dense = scenario2)` is called with `coarse` a
  variable holding a scenario
- **THEN** the design SHALL contain the two scenarios named `"coarse"` and
  `"dense"`, in input order

#### Scenario: Inconsistent name bindings abort
- **WHEN** two members bind the same `dataset` (or `min_pmix`, or `distset`) name
  to different values, or use different `partition_by`
- **THEN** `ssd_design()` SHALL abort with an informative error naming the
  offending binding

#### Scenario: Invalid elements and names abort
- **WHEN** `ssd_design()` is called with a non-`ssdsims_scenario` element, with
  duplicate/empty/unsafe names, or with no arguments
- **THEN** it SHALL abort with an informative error

### Requirement: A design is the de-duplicated union of its members' task sets
The package SHALL provide
`ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`,
which SHALL accept an `ssdsims_design` and return the complete list of `targets`
objects running every member in one static-branching pipeline, runnable by a
single `targets::tar_make()`. The factory SHALL union the members' per-step task tables
and **de-duplicate by task identity (the `<step>_id` cell key)**, so a cell
several members share resolves to **one** target computed **once**, and the
pipeline computes exactly the ragged union of cells the members request (never the
full cross-product). The factory SHALL place `...` immediately after `design` and
call `rlang::check_dots_empty()`. It SHALL perform no network I/O. The public
contract of `ssd_scenario_targets()` and the per-shard step runners SHALL be
unchanged (no ssdtools refactor).

#### Scenario: A refinement shares the coarse overlap
- **WHEN** a design unions a coarse member and a denser member that refines a
  subregion (extra axis values for one dataset/distset), under the same `seed`
- **THEN** every cell the two members share SHALL be a single target computed
  once, the denser member's extra cells SHALL build once, and the combined target
  list SHALL contain no duplicate names

#### Scenario: One tar_make runs the whole design
- **WHEN** a `_targets.R` builds `design <- ssd_design(a, b)`, calls
  `ssd_design_targets(design)`, and `targets::tar_make()` runs
- **THEN** every shard target the design requires SHALL build and the combined
  design summary SHALL be written

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_design_targets()` is called with a positional argument after
  `design`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error

### Requirement: Shards use naked cell addressing under seed and layout levels
The design factory SHALL address each shard at
`<root>/seed=<value>/layout=<hash(partition_by)>/<step>/<cells>` and name its
target `<step>_<cells>`, with **no** per-scenario path prefix, **no** `<name>_`
target-name prefix, and **no** opaque content key. The `seed=<value>` level SHALL
isolate members with different seeds (which share no draws) while members sharing
a `seed` share every coincident cell. Correctness of naked addressing SHALL rest
on the `ssd_design()` consistency contract and a uniform `partition_by`: at a given
`seed` and layout, the same cell SHALL denote byte-identical content.

#### Scenario: Distinct-seed members share nothing
- **WHEN** a design contains two members differing only in `seed`
- **THEN** their shards SHALL land under distinct `seed=<value>` trees and no
  shard SHALL be shared between them

#### Scenario: Same-seed members share coincident cells
- **WHEN** two same-`seed` members have a coincident cell (same `<step>_id`, same
  layout)
- **THEN** that cell SHALL be one target writing one path, read by both members

### Requirement: Design-pipeline results are byte-identical to standalone runs
A member's per-task `sample`, `fit`, and `hc` results SHALL be byte-identical (as
read-back R values) between `ssd_design_targets()` and a run of the same scenario
alone through `ssd_scenario_targets()`: combination changes addressing only
(target names and the `seed=` results level), never any task's `(seed, primer)`.
Scenario *names* SHALL NOT enter task identity, the primer, or any result value.

#### Scenario: Per-task results equal the standalone pipeline's
- **WHEN** a member is run both standalone and as part of a design, and per-task
  results are aligned by `<step>_id`
- **THEN** the `sample`, `fit`, and `hc` results SHALL be equal across the runs

### Requirement: dists is resolved design-wide as the union fit with a distset subset
The design factory SHALL rely on the existing single-scenario mechanism in which
`fit` fits the union `scenario$fit$dists` once per fit cell and `hc` carries
`distset` as an axis subsetting that union fit. Members requesting different
distribution sets SHALL therefore share the union fit shards and differ only by the
`distset` cell; `dists` SHALL NOT fork a `fit` shard. The design factory SHALL NOT
introduce a `dists` axis or refit per set.

#### Scenario: Different distsets share the fit shards
- **WHEN** two same-`seed` members differ only in their requested `distset`
  (consistently named per the contract)
- **THEN** they SHALL share every `sample` and `fit` shard, differing only in their
  `distset` hc cells

### Requirement: nrow_max is a uniform draw-size guard
`nrow_max` SHALL be treated as a uniform sample draw-size guard across a design.
The factory SHALL NOT aggregate `nrow_max`; a design whose members differ in
`nrow_max`, or that changes `nrow_max` between runs into the same root, SHALL be
documented as **undefined behaviour** (the stored sample bytes are not guaranteed
to coincide).

#### Scenario: Documented undefined behaviour for non-uniform nrow_max
- **WHEN** members of a design specify different `nrow_max` values
- **THEN** the documentation SHALL state that shard coincidence is not guaranteed
  and `nrow_max` SHOULD be held uniform

### Requirement: Members in a seed group must agree on hc readout settings
Members of a design that share a `seed` SHALL agree on the four non-axis hc
settings (`proportion`, `est_method`, `ci`, `samples`) — as well as on `nrow_max`
and the fit `dists` union — and the factory SHALL **abort** with an informative
error when they differ, rather than silently writing divergent bytes to a shared
cell. This change ships the ragged-grid (irregular-grid) primary driver and defers
the per-overlap hc readout aggregation to the follow-up `hc-readout-aggregation`
change, to which the abort message SHALL point. Members MAY still differ freely in
the **axes** (`nrow`, `dataset`, `sim`, `distset`, the fit grid), which is the
irregular-grid use this change delivers.

#### Scenario: Differing hc readouts abort with a pointer to the follow-up
- **WHEN** `ssd_design_targets(design)` is called for a design whose seed-group
  members differ in `proportion`, `est_method`, `ci`, or `samples`
- **THEN** it SHALL abort with an informative error stating the per-overlap
  aggregation is not yet supported

#### Scenario: Differing axis coverage is accepted
- **WHEN** seed-group members agree on the hc readout settings but differ in axis
  coverage (e.g. different `nrow` or `distset` values)
- **THEN** `ssd_design_targets(design)` SHALL build the ragged union of their cells
  without aborting

### Requirement: Single-scenario runs migrate to a design without recomputing
The package SHALL support migrating a single-scenario `ssd_scenario_targets()` run
to a design as a first-class, documented path: wrapping the scenario in
`ssd_design()` and calling `ssd_design_targets()` SHALL run that scenario as a
design of one. The migration SHALL be **cache-preserving** — a single-scenario run
and a design of one SHALL address their shards identically (the same
`scenario_results_dir()` seed-/layout-keyed root and the same `seed`-woven target
names), so re-running the design into the same root SHALL reuse every existing
shard (only the per-member and combined `summary` targets are new). To make this
hold, both `ssd_scenario_targets()` and `ssd_design_targets()` SHALL treat `root`
as the **base** directory and write shards under `scenario_results_dir(scenario,
root)`. Per-task `sample`/`fit`/`hc` results SHALL be byte-identical to the
standalone run (migration changes addressing only). The migration SHALL be
demonstrated by a dedicated vignette.

#### Scenario: A scenario migrated to a design of one reuses its shards
- **WHEN** a scenario is run via `ssd_scenario_targets(scenario, root = "results")`,
  then a design of one (`ssd_design_targets(ssd_design(scenario), root = "results")`)
  is sourced into the same store and root and `targets::tar_outdated()` is queried
- **THEN** no `sample`/`fit`/`hc` shard target SHALL be outdated (only the summary
  targets), so re-running recomputes no shard

#### Scenario: A scenario migrated to a design of one gives identical results
- **WHEN** a scenario is run via `ssd_scenario_targets()` and then via
  `ssd_design_targets(design)` for `design <- ssd_design(scenario)`, aligned by
  `<step>_id`
- **THEN** the `sample`/`fit`/`hc` per-task results SHALL be byte-identical across
  the two runs

#### Scenario: A migrated design grows by adding members
- **WHEN** a design of one is run to completion and then a refining member is added
  to the `ssd_design(...)` call and `tar_make()` re-run into the same root
- **THEN** the cells the new member shares (within the seed) SHALL be reported
  cached and only its extra cells (and the affected summaries) SHALL build

### Requirement: A combined design summary with a scenario identity column
The design pipeline SHALL include per-member `summary_<name>` targets that read the
shared shards and filter to each member's cells and `(proportion, est_method, ci,
samples)` readout slice, and one top-level `summary` target that names every
`summary_<name>` target and writes a combined `<root>/summary.parquet` unioning the
per-member **compact** summaries, each row tagged with a `scenario` column. The
union SHALL be performed at the DuckDB level (lazy reads written straight back out)
so no per-member summary is collected into R. Per-member compact summary files that
did not land SHALL be skipped (survivors union). Retained-draws
(`summary-samples.parquet`) files SHALL remain per-member and SHALL NOT be combined.

#### Scenario: Combined summary unions and tags the per-member summaries
- **WHEN** a design of members `a` and `b` runs to completion
- **THEN** `<root>/summary.parquet` SHALL contain the union of the two members'
  filtered compact summaries with a `scenario` column equal to `"a"` or `"b"` per
  row

#### Scenario: A member whose summary did not land is skipped
- **WHEN** one member's summary fails to land while the others succeed
- **THEN** the combined summary SHALL union the surviving members rather than
  aborting

### Requirement: Upload mirrors the addressed tree
With a non-`NULL` `upload`, `ssd_design_targets()` SHALL pair each shard with an
`upload_<step>` target (per the `cloud-upload` capability) whose destination is the
supplied upload object addressed by the **same** `seed=`/`layout=`/`<cells>` path
the local shard uses — with no per-scenario prefix — so a shared shard uploads
**once** and the remote layout mirrors the local one. With `upload = NULL` (the
default) the factory SHALL emit no upload targets. The per-task results SHALL be
unchanged and independent of `upload`.

#### Scenario: Shared shards upload once under their cell path
- **WHEN** `ssd_design_targets(design, upload = <destination>)` is called for a
  design whose members share shards
- **THEN** each shared shard SHALL have a single `upload_<step>` target whose
  destination path carries the shard's `seed=`/`layout=`/`<cells>` (no per-scenario
  level)

#### Scenario: upload defaults to no upload targets
- **WHEN** `ssd_design_targets(design)` is called without `upload`
- **THEN** the returned target list SHALL contain no `upload_<step>` targets
