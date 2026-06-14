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
- **WHEN** a `_targets.R` calls `ssd_design_targets(ssd_design(a, b))` and
  `targets::tar_make()` runs
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

### Requirement: hc readout settings are aggregated per overlapping cell
The design factory SHALL aggregate the four **non-axis** hc settings **per shared
hc cell, over only the members whose task set contains that cell**: `proportion`
and `est_method` SHALL be **unioned** and `ci` and `samples` reduced with
**`any()`**. A cell reached by only one member SHALL keep that member's demand, so
the bootstrap SHALL run only where a `ci = TRUE` member has tasks. The draw-shaping
axes `nboot`/`ci_method`/`parametric` and `distset` SHALL remain cell axes (in the
primer) and SHALL NOT be aggregated. A `ci = FALSE` task's analytical `est` SHALL
be served by a coincident `ci = TRUE` shard at the same `(fit-id, distset)` when
one exists, and only otherwise mint a standalone `ci = FALSE` shard. The set of
computed hc shards SHALL be every `ci = TRUE` member's cells plus the `ci = FALSE`
cells with no overlapping `ci = TRUE` shard, each carrying the unioned
`est_method`/`proportion` and `any` `samples` over the members it serves. This
SHALL reuse `ssdtools::ssd_hc()`'s vectorized `proportion`/`est_method` and
`hc_collapse_est_methods()` with no change to ssdtools or the per-shard hc runner,
and SHALL preserve byte-identity (the served `est` equals the standalone
`ci = FALSE` value; a `ci = TRUE` member's CI uses its own cell primer).

#### Scenario: Bootstrap only the overlapping cells
- **WHEN** a design unions `ci = FALSE, nsim = 1000` and `ci = TRUE, nsim = 10`
  (otherwise shareable, same `seed`)
- **THEN** the hc shards for the 10 overlapping sims SHALL be computed with
  `ci = TRUE`, the remaining 990 SHALL be computed `ci = FALSE`, and the
  `ci = FALSE` member SHALL read its (analytical, ci-invariant) `est` for the
  overlapping sims from the `ci = TRUE` shards

#### Scenario: Readout-only differences share the hc cell
- **WHEN** two same-`seed`, same-`dists` members at a coincident hc cell differ
  only in `proportion`/`est_method`/`ci`/`samples`
- **THEN** that cell SHALL be one shard carrying the union of their `proportion`/
  `est_method` and `any` of `ci`/`samples`, and each member's summary SHALL contain
  exactly its requested readout rows

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
