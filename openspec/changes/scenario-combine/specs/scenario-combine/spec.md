# scenario-combine Specification (delta)

## ADDED Requirements

### Requirement: A design is a named scenario collection
The package SHALL provide `ssd_design(...)` returning a classed `ssdsims_design`
named collection of `ssdsims_scenario` objects — the **design** (DoE sense: the
set of conditions to run), the union of regular per-scenario grids into one
possibly-non-regular design. Names SHALL be taken from explicit argument names or
derived from the argument expression (mirroring the `ssd_data()` dataset-name
derivation); each name is a **scenario name** within the design. The constructor
SHALL accept **one or more** scenarios — a design of a single scenario is valid
and SHALL be shaped exactly like any larger design — and SHALL abort on an empty
call. The constructor SHALL validate that every element is an `ssdsims_scenario`
and that the names are unique, non-empty, non-`NA`, and safe (starting with a
letter; letters, digits, and underscore only), aborting with an informative error
otherwise. A scenario name SHALL be used only as the `scenario` identity-column
value and the per-scenario summary target-name suffix — never in a shard path,
shard target name, the per-task primer, or any result value. Construction SHALL
be RNG-free (no draws; `.Random.seed` unchanged).

#### Scenario: Names are derived or explicit
- **WHEN** `ssd_design(base, wide = scenario2)` is called with `base` a variable
  holding a scenario
- **THEN** the design SHALL contain the two scenarios named `"base"` (from the
  argument expression) and `"wide"` (explicit), in input order

#### Scenario: A design of one is valid and uniformly shaped
- **WHEN** `ssd_design(base)` is called with a single scenario
- **THEN** it SHALL return a one-member design addressed exactly as in a larger
  design, while a no-argument `ssd_design()` call SHALL abort with an informative
  error

#### Scenario: Invalid elements and names abort
- **WHEN** `ssd_design()` is called with an element that is not an
  `ssdsims_scenario`, or with duplicate, empty, or unsafe names
- **THEN** it SHALL abort with an informative error identifying the offending
  element or name

### Requirement: A design target factory builds one pipeline
The package SHALL provide
`ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`
that accepts an `ssdsims_design` collection and returns the complete list of
`targets` objects running every scenario in the design in a single
static-branching pipeline, runnable by one `targets::tar_make()`. The factory
SHALL place `...` immediately after `design` and call `rlang::check_dots_empty()`,
so `root`, `upload`, and `cue` MUST be passed by name. The factory SHALL perform
no network I/O. The public contract of the single-scenario
`ssd_scenario_targets()` SHALL be unchanged, and the per-shard step runners and
the package's `ssdtools::ssd_hc()` usage SHALL be unchanged (no ssdtools
refactor).

#### Scenario: One tar_make runs the whole design
- **WHEN** a `_targets.R` builds two scenarios, calls
  `ssd_design_targets(ssd_design(a, b))`, and `targets::tar_make()` runs
- **THEN** every shard target the design requires SHALL build in the one
  pipeline, and the combined design summary SHALL be written

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_design_targets()` is called with a positional argument after
  `design`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error

### Requirement: Shards are addressed by per-step cumulative content keys
The design factory SHALL address each step's shards by a **per-step cumulative
content key** `sig`, writing each shard at `<root>/<step>/sig=<hash>/<cells>` (the
`<cells>` being the step's `partition_by` path-axis values) and minting its target
name from `<step>`, the `sig`, and the cells — with **no** `scenario=<name>` path
level and **no** `<name>_` target-name prefix. The `sig` SHALL be a digest of the
scenario `seed` together with exactly the scenario fields that step's runner
consumes (the `scenario_step_slice()` projection), cumulated down the dependency
graph so that `fit`'s sig depends on `sample`'s and `hc`'s on `fit`'s. Two
scenarios whose `(step, sig, cells)` coincide SHALL therefore mint the **same**
target writing the **same** path; two scenarios that differ in any input a step's
bytes depend on SHALL receive **distinct** sigs and SHALL NOT mix shards.

#### Scenario: The sig includes the seed
- **WHEN** two scenarios are identical except for their `seed`
- **THEN** their `sample` (and hence `fit` and `hc`) sigs SHALL differ, so no
  shard is shared between them

#### Scenario: Differing off-axis settings fork the sig
- **WHEN** two scenarios differ in an off-axis setting that changes a step's
  bytes (e.g. `dists` at the `fit` step, or `nrow_max` at the `sample` step)
- **THEN** that step's sig (and every downstream step's sig) SHALL differ, so the
  affected shards are not shared

### Requirement: Identical shards are shared across scenarios
The design factory SHALL share a shard across scenarios when two scenarios in a
design produce a byte-identical shard for a step (same `seed` and same step
content, hence the same `sig` and `cells`): it SHALL emit **one** target for that
shard, `tar_make()` SHALL compute it **once**, and every scenario that needs it
SHALL read that single shard. In particular,
because `nrow_max` is a uniform draw-size guard and the primer is seed-anchored,
all same-`seed` scenarios in a design SHALL share every `sample` shard, and
scenarios that additionally agree on `dists` SHALL share their `fit` shards.
Sharing SHALL be licensed by byte-identity: a shared shard SHALL equal the result
each sharing scenario would have computed alone.

#### Scenario: A downstream comparison shares upstream shards
- **WHEN** a design compares two scenarios that differ only in a downstream
  setting (e.g. `est_method`) under the same `seed`
- **THEN** they SHALL share all `sample` and `fit` shards — each computed once —
  and the combined pipeline SHALL contain no duplicate target names

#### Scenario: A dists comparison shares sample but forks fit
- **WHEN** a design compares two scenarios differing only in `dists`
- **THEN** they SHALL share every `sample` shard while their `fit` (and `hc`)
  shards land under distinct `sig` paths

### Requirement: hc readout settings are aggregated into shared hc shards
The design factory SHALL compute the seed-free hc readout settings as a maximal
set in the shared `hc` shards rather than forking the `hc` sig per scenario:
`proportion` and `est_method` SHALL be **unioned** across the design, and `ci`
and `samples` SHALL be reduced with **`any()`**. Consequently `hc_sig` SHALL key
only the seed-dependent draw-shapers (the parent `fit` lineage and the
`nboot`/`ci_method`/`parametric`/`distset` cells), NOT the readout settings. Each
scenario's summary SHALL filter the shared `hc` shards down to that scenario's
`(proportion, est_method, ci, samples)` slice. This aggregation SHALL reuse
`ssdtools::ssd_hc()`'s vectorized `proportion`/`est_method` and the package's
`hc_collapse_est_methods()` (one bootstrap, analytical `est` per method) with no
change to ssdtools or to the per-shard hc runner.

#### Scenario: Readout-only differences share the hc shard
- **WHEN** two scenarios in a design differ only in `proportion`, `est_method`,
  `ci`, or `samples` (same `seed`, same `dists`)
- **THEN** they SHALL share the same `hc` shards (computed once over the union of
  `proportion`/`est_method` and `any()` of `ci`/`samples`), and each scenario's
  summary SHALL contain exactly its requested readout rows

#### Scenario: A ci=FALSE scenario reads the point estimate from a shared ci=TRUE shard
- **WHEN** a design contains one `ci = TRUE` and one `ci = FALSE` scenario that
  are otherwise shareable
- **THEN** the shared `hc` shard SHALL be computed with `ci = TRUE`, the
  `ci = FALSE` scenario's summary SHALL read only the (ci-invariant) point `est`,
  and that `est` SHALL equal what the scenario would have computed with
  `ci = FALSE`

### Requirement: Design-pipeline results are byte-identical to standalone runs
A scenario's per-task `sample`, `fit`, and `hc` results SHALL be byte-identical
(as read-back R values) between `ssd_design_targets()` and a run of the same
scenario alone through `ssd_scenario_targets()`: combination changes addressing
only (target names and results roots), never any task's `(seed, primer)`. Scenario
*names* SHALL NOT enter task identity, the per-task primer, or any result value.

#### Scenario: Per-task results equal the standalone pipeline's
- **WHEN** a scenario is run both standalone (`ssd_scenario_targets()`) and as a
  member of a design (`ssd_design_targets()`), and the per-task results are
  aligned by the task-identity key (`<step>_id`)
- **THEN** the `sample`, `fit`, and `hc` results SHALL be equal across the two
  runs

#### Scenario: Renaming a scenario leaves results and shards unchanged
- **WHEN** the same scenario object is placed in a design under two different
  names (in separate runs)
- **THEN** the per-task results, shard target names, and `sig` paths SHALL be
  identical across the two runs (only the `scenario` summary column and the
  `summary_<name>` target name differ)

### Requirement: Design growth mints only the unshared targets
Design growth SHALL be additive at the shard layer: adding a scenario to a design
that has already been `tar_make()`'d into a root SHALL mint new shard targets only
for the `(sig, cells)` the new member does not already share with an existing
member; every shard the new member shares SHALL be reported cached (skipped) with
its Parquet byte-identical. Widening the hc readout union (a new `proportion`/
`est_method`, or the first `ci`/`samples` set to `TRUE`) SHALL re-sig and rebuild
the `hc` shards while leaving every `sample` and `fit` shard cached. Removing a
member SHALL leave every shard still required by a remaining member cached, with
shards no longer required abandoned in place. The documentation SHALL document
promoting a standalone `ssd_scenario_targets()` run into a design as **safe but
recomputing** (task identity and `(seed, primer)` unchanged; addressing changes
from `layout=` to `sig=`).

#### Scenario: Adding a downstream-only member reuses upstream shards
- **WHEN** a design of scenario `a` is run to completion, then grown to
  `ssd_design(a, b)` where `b` differs from `a` only in a readout setting, and
  `tar_make()` is re-run into the same root
- **THEN** every `sample` and `fit` shard SHALL be reported cached, and (because
  the hc readout union widened) only the `hc` shards and the affected summaries
  SHALL rebuild

#### Scenario: Adding a member that shares nothing builds its own shards
- **WHEN** a design is grown by a member with a distinct `seed`
- **THEN** the new member's `sample`/`fit`/`hc` shards SHALL build under their own
  sigs and every pre-existing shard SHALL be reported cached

### Requirement: A combined design summary with a scenario identity column
The design pipeline SHALL include one top-level `summary` target (the only target
name carrying no `sig`) that names every per-scenario `summary_<name>` target and
writes a combined `<root>/summary.parquet` unioning the per-scenario **compact**
summaries, each row tagged with a `scenario` column holding the scenario's name.
The union SHALL be performed at the DuckDB level (lazy reads written straight back
out) so no per-scenario summary is collected into R. Per-scenario compact summary
files that did not land SHALL be skipped, so the combined summary unions the
surviving scenarios. Retained-draws (`summary-samples.parquet`) files SHALL remain
per-scenario and SHALL NOT be combined.

#### Scenario: Combined summary unions and tags the per-scenario summaries
- **WHEN** a design of scenarios `a` and `b` runs to completion
- **THEN** `<root>/summary.parquet` SHALL contain the union of the two scenarios'
  filtered compact summaries with a `scenario` column equal to `"a"` or `"b"` per
  row

#### Scenario: A scenario whose summary did not land is skipped
- **WHEN** one scenario's summary fails to land while the others succeed
- **THEN** the combined summary SHALL union the surviving scenarios rather than
  aborting

### Requirement: Upload mirrors the content-keyed tree
With a non-`NULL` `upload`, `ssd_design_targets()` SHALL pair each shard with an
`upload_<step>` target (per the `cloud-upload` capability) whose destination is
the supplied upload object addressed by the **same** `sig=<hash>/<cells>` path the
local shard uses — with no `scenario=<name>` prefix extension — so a shard shared
by several scenarios uploads **once** and the remote layout mirrors the local one.
With `upload = NULL` (the default) the factory SHALL emit no upload targets. The
per-task results SHALL be unchanged and independent of `upload`.

#### Scenario: Shared shards upload once under their sig path
- **WHEN** `ssd_design_targets(design, upload = <destination>)` is called for a
  design whose scenarios share shards
- **THEN** each shared shard SHALL have a single `upload_<step>` target whose
  destination path carries the shard's `sig=<hash>/<cells>` (no `scenario=` level)

#### Scenario: upload defaults to no upload targets
- **WHEN** `ssd_design_targets(design)` is called without `upload`
- **THEN** the returned target list SHALL contain no `upload_<step>` targets
