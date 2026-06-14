# task-shards Specification (delta)

## MODIFIED Requirements

### Requirement: Results written under a per-layout root keyed by partition_by
`ssd_scenario_targets()` SHALL write each step's shards under a results path that
is a **pure function of content**: the `partition_by`/`bundle` layout (whose Hive
depth/axes it already determines) **plus**, at the step where it bites, every
scalar simulation setting that changes that step's output bytes (`nrow_max` at
`sample`, `est_method` at the hc `summarise` step). A setting that does not affect
a step SHALL NOT appear in that step's path, and a discriminating setting SHALL be
present **always**, not conditionally, so the path is a function of the shard
alone. Re-running with a changed `partition_by`/`bundle` SHALL still write to a
fresh layout root and never mix shard granularities; a non-layout,
non-discriminating knob (e.g. `seed`) SHALL NOT change the path.

#### Scenario: Different partition_by yields a different results root
- **WHEN** the results path is computed for two scenarios that differ only in
  `partition_by`
- **THEN** the two layout roots SHALL differ

#### Scenario: A discriminating setting is recorded in the path
- **WHEN** the results path is computed for two scenarios that differ only in
  `est_method`
- **THEN** their `summarise` (hc) paths SHALL differ by an `est_method`
  discriminator while their `sample`, `fit`, and `draw` paths SHALL be identical

## ADDED Requirements

### Requirement: The target factory addresses targets by content
`ssd_scenario_targets()` SHALL name each step's shard target by a pure function of
content — its `partition_by` path cells plus the scalar setting discriminators
that bite at that step — and SHALL NOT incorporate any scenario identity into the
name. The factory SHALL also split the `hc` step into a content-addressed `draw`
target and an RNG-free `summarise` target (per the `hazard-concentrations`
capability), wiring `draw → summarise` through the per-child upstream edges. The
per-task results SHALL be byte-identical as read-back values to the pre-change
pipeline; only the layout (settings in the path, the `hc` split) changes.

#### Scenario: Target names are a function of content
- **WHEN** the factory mints a step's shard targets
- **THEN** each target name SHALL be derived from the step's path cells and the
  discriminating settings that bite at that step, with no scenario identity

#### Scenario: hc is emitted as draw plus summarise
- **WHEN** `ssd_scenario_targets()` builds the `hc` step with `ci = TRUE`
- **THEN** it SHALL emit a content-addressed `draw` target and an RNG-free
  `summarise` target, the latter reading the former
