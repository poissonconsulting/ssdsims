# scenario-combine Specification (delta)

## ADDED Requirements

### Requirement: A named scenario collection constructor
The package SHALL provide `ssd_scenarios(...)` returning a classed
`ssdsims_scenarios` named collection of `ssdsims_scenario` objects. Names SHALL
be taken from explicit argument names or derived from the argument expression
(mirroring the `ssd_data()` dataset-name derivation). The constructor SHALL
validate that every element is an `ssdsims_scenario` and that the names are
unique, non-empty, non-`NA`, and safe to serve as both a target-name prefix and
a results directory level (starting with a letter; letters, digits, and
underscore only), aborting with an informative error otherwise. Construction
SHALL be RNG-free (no draws; `.Random.seed` unchanged).

#### Scenario: Names are derived or explicit
- **WHEN** `ssd_scenarios(base, wide = scenario2)` is called with `base` a
  variable holding a scenario
- **THEN** the collection SHALL contain the two scenarios named `"base"` (from
  the argument expression) and `"wide"` (explicit), in input order

#### Scenario: Invalid elements and names abort
- **WHEN** `ssd_scenarios()` is called with an element that is not an
  `ssdsims_scenario`, or with duplicate, empty, or unsafe names (e.g. a name
  not matching the documented safe shape)
- **THEN** it SHALL abort with an informative error identifying the offending
  element or name

### Requirement: A multi-scenario target factory builds one pipeline
The package SHALL provide
`ssd_scenarios_targets(scenarios, ..., root = "results", upload = NULL, cue = NULL)`
that accepts an `ssdsims_scenarios` collection and returns the complete list of
`targets` objects running every scenario in a single static-branching pipeline.
For each named scenario it SHALL emit the same target set
`ssd_scenario_targets()` builds for that scenario — the per-step shard targets,
the per-child upstream edges, and the per-scenario summary — with every target
name prefixed `<name>_` (so target names are disjoint across scenarios) and
written under the per-scenario, per-layout root
`<root>/scenario=<name>/layout=<hash>`. The factory SHALL place `...`
immediately after `scenarios` and call `rlang::check_dots_empty()`, so `root`,
`upload`, and `cue` MUST be passed by name. The factory SHALL perform no
network I/O. The public contract of the single-scenario
`ssd_scenario_targets()` SHALL be unchanged.

#### Scenario: One tar_make runs all scenarios
- **WHEN** a `_targets.R` builds two scenarios, calls
  `ssd_scenarios_targets(ssd_scenarios(a, b))`, and `targets::tar_make()` runs
- **THEN** every shard target of both scenarios SHALL build in the one
  pipeline, each scenario's shards and summary landing under its own
  `<root>/scenario=<name>/layout=<hash>` tree

#### Scenario: Target names are disjoint across scenarios
- **WHEN** two scenarios in the collection share path cells (e.g. the same
  dataset and `sim` values) and the pipeline is sourced
- **THEN** each scenario's targets SHALL carry its `<name>_` prefix and the
  combined target list SHALL contain no duplicate target names

#### Scenario: Scenarios sharing a layout do not mix shards
- **WHEN** two scenarios in the collection have identical `partition_by` (the
  same layout hash)
- **THEN** their shards SHALL land under distinct `scenario=<name>` subtrees
  and neither scenario's readers SHALL union the other scenario's shards

#### Scenario: root, upload, and cue must be passed by name
- **WHEN** `ssd_scenarios_targets()` is called with a positional argument
  after `scenarios`, or a misspelled named argument
- **THEN** `rlang::check_dots_empty()` SHALL abort with an informative error

### Requirement: Combined-pipeline results are byte-identical to standalone runs
A scenario's per-task `sample`, `fit`, and `hc` results produced through
`ssd_scenarios_targets()` SHALL be byte-identical (as read-back R values) to
those produced by running the same scenario alone through
`ssd_scenario_targets()`: combination changes addressing only (target names
and results roots), never any task's `(seed, primer)`. Scenario *names* SHALL
NOT enter task identity, the per-task primer, or any result value. Two
scenarios sharing a `seed` SHALL share `(seed, primer)` on overlapping task
identities (common random numbers for paired settings comparisons); this
SHALL be documented and SHALL NOT be warned about or rejected.

#### Scenario: Per-task results equal the standalone pipeline's
- **WHEN** a scenario is run both standalone (`ssd_scenario_targets()`) and as
  a member of a combined pipeline (`ssd_scenarios_targets()`), and the
  per-task results are aligned by the task-identity key (`<step>_id`)
- **THEN** the `sample`, `fit`, and `hc` results SHALL be equal across the two
  runs

#### Scenario: Renaming a scenario leaves results unchanged
- **WHEN** the same scenario object is combined under two different collection
  names (in separate runs)
- **THEN** the per-task results SHALL be identical across the two runs, with
  only the target names and `scenario=<name>` paths differing

### Requirement: A combined cross-scenario summary with a scenario identity column
The multi-scenario pipeline SHALL include one top-level `summary` target (the
only unprefixed target the factory mints) that names every per-scenario
summary target (ordering and invalidation) and writes a combined
`<root>/summary.parquet` unioning the per-scenario **compact** summaries, each
row tagged with a `scenario` column holding the scenario's collection name.
The union SHALL be performed at the DuckDB level (lazy reads written straight
back out) so no per-scenario summary is collected into R. Per-scenario compact
summary files that did not land SHALL be skipped, so the combined summary
unions the surviving scenarios (the §6.2 keep-going property). Retained-draws
(`summary-samples.parquet`) files SHALL remain per-scenario and SHALL NOT be
combined.

#### Scenario: Combined summary unions and tags the per-scenario summaries
- **WHEN** a combined pipeline of scenarios `a` and `b` runs to completion
- **THEN** `<root>/summary.parquet` SHALL contain the union of the two
  per-scenario compact summaries with a `scenario` column equal to `"a"` or
  `"b"` per row, and each per-scenario `summary.parquet` SHALL be unchanged by
  the combination

#### Scenario: A scenario whose summary did not land is skipped
- **WHEN** one scenario's summary fails to land while the others succeed
- **THEN** the combined summary SHALL union the surviving scenarios' compact
  summaries rather than aborting

### Requirement: Per-scenario upload prefixing
With a non-`NULL` `upload`, `ssd_scenarios_targets()` SHALL pair each
scenario's step shards with `upload_<step>` targets (per the `cloud-upload`
capability) whose destination is the supplied upload object with its blob
prefix extended by `scenario=<name>`, so the remote layout mirrors the local
`scenario=<name>` trees and no two scenarios upload to the same blob path.
With `upload = NULL` (the default) the factory SHALL emit no upload targets.
The per-task results SHALL be unchanged and independent of `upload`.

#### Scenario: Each scenario uploads under its own prefix
- **WHEN** `ssd_scenarios_targets(scenarios, upload = <destination>)` is
  called for scenarios `a` and `b`
- **THEN** the returned list SHALL contain per-shard upload targets whose
  destinations carry blob prefixes ending `scenario=a` and `scenario=b`
  respectively (extending any prefix already on the destination)

#### Scenario: upload defaults to no upload targets
- **WHEN** `ssd_scenarios_targets(scenarios)` is called without `upload`
- **THEN** the returned target list SHALL contain no `upload_<step>` targets
