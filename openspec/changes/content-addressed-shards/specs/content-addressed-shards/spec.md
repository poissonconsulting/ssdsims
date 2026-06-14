# content-addressed-shards Specification (delta)

## ADDED Requirements

### Requirement: Shard addressing is a pure function of content
A shard's `targets` target name and its Hive storage path SHALL be a pure
function of the shard's **content** — its `partition_by` path cells plus, at the
step where it bites, every scalar simulation setting that changes that step's
output bytes — and SHALL carry **no scenario identity** (no `scenario=<name>`
directory level and no `<name>_` target-name prefix). The discriminating setting
SHALL be present at the addressing of the step it affects **always**, never
conditional on whether it takes more than one value, so the address depends on
the shard alone and not on any other scenario it might be run alongside. A setting
that does not affect a given step SHALL NOT appear in that step's address (so
upstream steps stay shared). Identical content SHALL therefore resolve to the
identical target name and path, and distinct content SHALL resolve to distinct
names and paths. This is the **design-ready** property a later design layer
composes on (`scenario-combine-v2`): when that layer emits these targets for
several scenarios, identical content already collides at one address.

#### Scenario: Identical content yields one address
- **WHEN** the target name and storage path are computed for a shard whose
  content (layout, partition cell, and discriminating settings) is identical,
  reached via two different scenario configurations
- **THEN** the target name and path SHALL be identical in both cases, so a
  composing design layer would build it once

#### Scenario: A discriminating setting appears only at the step it affects
- **WHEN** two scenario configurations differ only in `est_method`
- **THEN** the `est_method` discriminator SHALL appear in the `summarise` (hc)
  step's name and path but NOT in the `sample`, `fit`, or `draw` addresses, so
  the upstream addresses are identical

#### Scenario: No scenario identity enters the address
- **WHEN** any shard's target name and path are computed by the
  `ssd_scenario_targets()` factory
- **THEN** neither SHALL contain a `scenario=<name>` level nor a `<name>_`
  prefix; both SHALL be functions of content alone
