# scenario-combine-v2 Specification (delta)

## ADDED Requirements

### Requirement: A design is a named scenario collection whose names are selection labels
The package SHALL provide `ssd_design(...)` returning a classed `ssdsims_design`
named collection of `ssdsims_scenario` objects — the **design** (a named set of
scenarios run as one pipeline). Names SHALL be taken from explicit argument names
or derived from the argument expression (mirroring `ssd_scenario_data()`). Each
name is a **selection label** for the combined results and the derived membership
mapping and SHALL NOT enter any target name or storage path (which are
content-pure — the factory never puts a name in the address). The constructor
SHALL accept one or more
scenarios (a design of one is valid and uniformly shaped; an empty call aborts),
SHALL validate that every element is an `ssdsims_scenario` and that names are
unique, non-empty, and non-`NA`, and SHALL be RNG-free (`.Random.seed` unchanged).

#### Scenario: Names are derived or explicit and label selections
- **WHEN** `ssd_design(base, wide = scenario2)` is called with `base` a variable
  holding a scenario
- **THEN** the design SHALL contain the two scenarios labelled `"base"` (from the
  argument expression) and `"wide"` (explicit), in input order, and these labels
  SHALL NOT appear in any target name or storage path

#### Scenario: A design of one is valid; an empty call aborts
- **WHEN** `ssd_design(base)` is called with a single scenario, or `ssd_design()`
  with none
- **THEN** the single-scenario call SHALL return a one-member design uniformly
  shaped, and the empty call SHALL abort with an informative error

#### Scenario: Invalid elements or names abort
- **WHEN** `ssd_design()` is called with a non-`ssdsims_scenario` element, or with
  duplicate, empty, or `NA` names
- **THEN** it SHALL abort with an informative error identifying the offending
  element or name

### Requirement: A design factory composes the existing per-scenario targets into one deduplicated pipeline
The package SHALL provide
`ssd_design_targets(design, ..., root = "results", upload = NULL, cue = NULL)`
that accepts an `ssdsims_design` and returns the complete list of `targets`
objects running every member in one static-branching pipeline, by composing the
target set `ssd_scenario_targets()` already builds for each member and projecting
each member to the per-shard content so a shard's target identity — name *and*
command — is a pure function of its content. Members that share content SHALL emit
the identical target (same name, same path, same command), which the pipeline
builds **once**; members with distinct content SHALL emit distinct targets. The
factory SHALL place `...` immediately after `design` and call
`rlang::check_dots_empty()` (so `root`, `upload`, `cue` are by name) and SHALL
perform no network I/O. No per-scenario root or target-name prefix SHALL be
introduced.

#### Scenario: One tar_make runs the whole design, sharing common shards
- **WHEN** a `_targets.R` calls `ssd_design_targets(ssd_design(a, b))` and
  `targets::tar_make()` runs, where `a` and `b` share some shards
- **THEN** every unique shard SHALL build once in the one pipeline, each shared
  shard appearing as a single target built a single time

#### Scenario: Shared ancestors are one target, distinct cells two
- **WHEN** two members differ only in a `distset` axis value (an hc axis, so their
  `sample` and `fit` cells are identical and only their `hc` cells differ)
- **THEN** their `sample` and `fit` targets SHALL each be a single shared target,
  and only their `hc` targets SHALL be distinct

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_design_targets()` is called with a positional argument after
  `design`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error

### Requirement: Each unique shard is computed exactly once and a scenario extends into a design without recomputation
Building a design SHALL compute each unique shard exactly once. Wrapping a
completed standalone scenario in a design, or adding a member to a completed
design, SHALL build only the genuinely-new shards and SHALL report every shared
shard cached with its Parquet byte-identical to before; removing a member SHALL
leave the remaining shards cached. This follows because the composed targets'
identity is a pure function of content (the existing addressing carries no
scenario identity), so a member's shards resolve to the identical names and paths
regardless of the design they are part of — the `path-axis-growth` contract lifted
one level. Only the combined summary SHALL
re-run, over the current members. Two members sharing `(seed, primer)` on an
overlapping task SHALL resolve to the **same shard** (common random numbers,
computed once); this SHALL be documented and SHALL NOT be warned about.

#### Scenario: Standalone scenario wrapped in a design is fully cached
- **WHEN** a scenario is run to completion standalone, then run as a member of a
  design via `ssd_design_targets()` into the same root
- **THEN** every one of that scenario's shard targets SHALL be reported cached
  (skipped) with byte-identical Parquets; no per-task result SHALL recompute

#### Scenario: Adding a member builds only its non-shared shards
- **WHEN** a completed design is grown by a member sharing some shards with
  existing members
- **THEN** only the added member's non-shared shards (and the combined summary)
  SHALL build; every shared shard SHALL be reported cached and byte-identical

### Requirement: A combined design summary keyed by partition coordinates
The design pipeline SHALL include one top-level `summary` target that writes a
combined `<root>/summary.parquet` unioning the per-coordinate compact summaries,
keyed by **partition coordinates** (e.g. `dataset`, axis values including
`distset`), and SHALL NOT carry a `scenario` identity column. The union SHALL be
performed at the DuckDB level (lazy reads written straight back out) with no
per-shard summary collected into R, and SHALL skip coordinate summaries that did
not land (survivors-union). Scenario membership — which scenarios' selections
cover a coordinate — SHALL be a **derived** mapping (shared with
`cost-analysis-targets`), never stored on or beside a shard. Retained bootstrap
samples SHALL remain per coordinate and SHALL NOT be combined.

#### Scenario: Combined summary unions coordinates without a scenario column
- **WHEN** a design of overlapping scenarios runs to completion
- **THEN** `<root>/summary.parquet` SHALL contain one set of rows per distinct
  partition coordinate (a coordinate shared by several members appearing once),
  with no `scenario` column

#### Scenario: A coordinate whose summary did not land is skipped
- **WHEN** one coordinate's summary fails to land while others succeed
- **THEN** the combined summary SHALL union the survivors rather than aborting

#### Scenario: Membership is derived, not stored on a shard
- **WHEN** a new member is added whose selection covers a shard an existing member
  already produced
- **THEN** that shard's bytes SHALL be unchanged (no membership written to it),
  and scenario membership SHALL be obtainable as a derived query over the
  coordinate space
