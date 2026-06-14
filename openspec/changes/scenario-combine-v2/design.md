# Design: scenario-combine-v2

## Context

The original `scenario-combine` (archived as reference, specs unsynced) built the
**design** — `ssd_design()` + `ssd_design_targets()` + a combined summary — by
giving each member its own `scenario=<name>/layout=<hash>` tree and `<name>_`
target prefix. That was the only way to run two scenarios in one `_targets.R`
*while shards were addressed by `partition_by` alone* (two scenarios over the same
cells would otherwise collide on target names and Hive roots).

That constraint never really held: a shard's target name and Hive path are
**already** a pure function of content — the `partition_by` cells under
`layout=<hash(partition_by)>`, with no scenario identity — and the `GLOSSARY`
*simulation setting* contract already keeps `est_method`/`ci`/`nrow_max` out of
the address (a setting "never... becomes a shard/partition level"). So *identical
content already resolves to one target name and path*. (A `content-addressed-shards`
proposal to *add* settings to the address was explored this cycle and found moot —
it contradicted that contract — and is archived.)

This change builds the design layer on that existing foundation. It needs no
per-scenario decoupling: composing the members' existing targets into one pipeline
makes shared shards collapse to one target automatically. A "design" is a named
selection of scenarios over one shared results tree.

`distset-hc-axis` has already made `distset` an hc axis, so distribution-set pools
ride the ordinary axis machinery and a pool comparison is just members selecting
different `distset` values — shared upstream, distinct only at hc.

## Goals / Non-Goals

**Goals:**

- One `_targets.R`, one store, one `tar_make()` for a whole design, sharing the
  `crew` controller and the keep-going `error` policy.
- **Exactly-once:** each unique shard across the design's members is built once
  (shared content ⇒ one target), by composing the existing per-scenario targets
  whose identity is already a pure function of content.
- **Extend/grow without recomputation:** wrapping a completed scenario in a
  design, or adding/removing a member, builds only genuinely-new shards and
  caches the rest byte-identically.
- A combined summary keyed by **partition coordinates** (no `scenario` column),
  DuckDB-level, survivors-union.
- `ssd_design()` owns naming as **selection labels** (not addressing).

**Non-Goals:**

- **Changing the addressing.** The single-scenario factory's content-pure
  addressing is unchanged; this change only composes it. No new target-name or
  path scheme is defined here.
- **Per-setting sharing of a shared cell.** Two members differing only in a scalar
  setting (e.g. `est_method`) at the same cell are not specially addressed; the
  shard reflects whatever was computed and is recomputed on change (decision
  below). `distset` comparison is unaffected — it is an axis, so it yields
  distinct cells that share their ancestors naturally.
- **The materialised scenario→selection mapping.** A scenario's membership is a
  derived query over the coordinate space, shared with `cost-analysis-targets`;
  here it is stubbed, never stored on a shard.
- **The study level.** A *study* (longitudinal aggregate across runs) stays a
  future read-side concept, per `GLOSSARY.md`.
- **A `name` field on the scenario object.** Names live on the design collection
  (the `ssd_scenario_data()` pattern); the scenario's declarative identity is
  untouched.

## Decisions

### Decision: a design composes the existing per-scenario targets

`ssd_design_targets()` loops the design's named scenarios and emits, for each, the
target set `ssd_scenario_targets()` already builds, then appends one
combined-summary target. Each member is projected to the per-shard content so a
shard's target **identity** — name *and* command — is a pure function of its
content; members that share content thus emit the **same** target, which `targets`
de-duplicates to a single build, while members that differ (different axes/cells)
emit distinct targets. No merged "multi-scenario" object and no per-scenario root
or prefix exist.

*Alternative considered — the original `scenario-combine`'s `scenario=<name>`
trees and `<name>_` prefixes:* rejected and archived; it recomputes shared shards
once per member and makes a shard's address depend on its scenario, blocking dedup
and extend-without-recompute.

*Alternative considered — adding settings to the address (`content-addressed-shards`):*
explored this cycle and rejected as moot — the `GLOSSARY` already forbids a
setting from becoming a partition level, and single-scenario addressing already
carries no scenario identity, so there was nothing to add. Archived.

*Alternative considered — a mega-scenario whose settings become axes:* rejected;
`est_method`/`ci`/`nrow_max` stay scalar settings (and `distset` is already its
own hc axis via `distset-hc-axis`).

### Decision: scalar-setting divergence at a shared cell is not specially handled

If two members differ only in a scalar setting (e.g. `est_method`) at the same
cell, v2 adds **no** per-setting addressing. The shard reflects whatever the
pipeline computed; the result is self-evident from its own columns, and `targets`
recomputes and overwrites it when the command changes. We deliberately do not
engineer collision-free sharing for a scalar-setting comparison (it would mean
either re-adding settings to the address, rejected above, or computing a
superset nobody asked for). The intended comparison dimension, `distset`, is an
**axis** and so sidesteps this entirely (distinct cells, shared ancestors).

### Decision: `ssd_design(...)` owns naming as selection labels

A collection constructor returns a classed `ssdsims_design` named list of
scenarios. Names come from explicit argument names or are derived from the
argument expression (the `ssd_scenario_data()` derivation), validated unique,
non-empty, and non-`NA`. Names are **selection labels** — they tag the combined
results and drive the derived membership mapping — and crucially do **not** enter
target names or storage paths (those are content-pure; the factory never puts a
name in the address). The path-safety / symbol-fragment constraints the original
`scenario-combine` imposed
(because names became a `scenario=` level and a `<name>_` prefix) therefore relax;
a light unique/non-empty/non-`NA` check suffices, though a conservative safe shape
remains acceptable. `ssd_design_targets()` accepts **only** this collection.

*Alternative considered — accept a bare named list:* rejected; two entry points to
the same validation, inconsistent with the package's collection-owns-naming
direction.

### Decision: exactly-once and extend/grow without recomputation

Each unique shard is built once because shared content resolves to one target.
Wrapping a completed standalone scenario in a design, or adding a member, resolves
that scenario's shards to the **identical** target names and paths, so `tar_make()`
reports them cached and only the genuinely-new shards build — the
`path-axis-growth` contract lifted one level (appending a *scenario* is "more
named targets"). Removing a member leaves the remaining shards cached; a shard no
longer referenced by any member falls out of the graph and is reclaimed by
`tar_prune()` (refcount-for-free). Only the combined summary re-runs, over the
surviving members.

### Decision: the combined summary is keyed by coordinates, membership derived

A top-level `summary` target unions the per-coordinate compact summaries into
`<root>/summary.parquet` at the DuckDB level (lazy reads, no R materialise),
keyed by partition coordinates (`dataset`, axis values) with **no `scenario`
column**. A coordinate summary that did not land is skipped
(survivors-union). Scenario membership — which scenarios' selections cover each
coordinate — is **derived** on the read side, never stored on a shard: storing it
would force rewriting an unchanged shard each time a new member selected it. The
membership derivation is shared with `cost-analysis-targets`.

*Alternative considered — a `scenario` column / membership rows / list-column on
the summary (the original `scenario-combine`):* rejected; all bind mutable
membership to immutable shared content, and with overlapping members a single
shared coordinate cannot carry one scenario name.

### Decision: shared seeds are common random numbers, strengthened

Two members sharing `seed` and an overlapping task identity share the **same
shard** (not merely paired streams) — computed once. That is the strongest form of
the variance-reduction pairing for paired comparisons. The factory neither warns
nor validates seed distinctness; distinct seeds give independent streams as always.
Documented on `ssd_design_targets()`.

## Risks / Trade-offs

- **A scenario is a query, not a directory.** Reading "scenario A" needs the
  derived selection layer (shared with `cost-analysis-targets`). Until it lands,
  results are addressable by coordinate but not yet by scenario name.
- **Sourcing cost scales with the union of shard counts** — already the
  single-scenario model; tiny task tables, no new mechanism.
- **`union_all` schema drift** — the combined summary unions *this pipeline's*
  fresh per-coordinate summaries (one package version), not historical files;
  cross-version aggregation is the study level's problem.

## Migration Plan

No prerequisite change — rests on the existing addressing and on `distset-hc-axis`
(landed). Two new exports; no breaking change to existing exports; no
re-baselining (per-task results are unchanged — they are the existing shards this
layer composes). The original `scenario-combine` and the explored
`content-addressed-shards` are archived as reference (specs unsynced). Roll back by
reverting the commit.

## Open Questions

- Whether to ship a design `inst/targets-templates/` template or fold a design
  section into the existing template and the vignette — resolved during
  implementation (leaning: vignette section + extend the existing template).
