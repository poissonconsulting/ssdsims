# Proposal: scenario-combine-v2

## Why

A simulation study routinely runs several `ssd_scenario` objects that share
ground — the same datasets, overlapping axes (e.g. different `distset` pools, now
a real hc axis via `distset-hc-axis`), or staged growth of one scenario. The
ROADMAP books this as ‼️ [scenario-combine]: *provide a convenient way to run
multiple `ssd_scenario` objects as a single targets pipeline.* The original
`scenario-combine` gave each member its own `scenario=<name>` results tree and
`<name>_`-prefixed targets — making a design **N independent pipelines sharing a
`tar_make()`**, recomputing every shared shard once per member. That decoupling
is **archived as reference**.

It is unnecessary because shard addressing is **already content-pure**:
`ssd_scenario_targets()` names targets and roots shards by their partition cells
under `layout=<hash(partition_by)>`, with **no scenario identity**, and the
`GLOSSARY` *simulation setting* contract already keeps `est_method`/`ci`/`nrow_max`
out of the address entirely (a setting "never... becomes a shard/partition
level"). So two scenarios that share content already resolve to the same target
name and path. (A short-lived `content-addressed-shards` proposal to *add*
settings to the address was explored and found moot — it contradicted that
contract — and is archived.)

This **v2** therefore builds only the design layer, composing the existing
per-scenario targets. Running several scenarios in one pipeline shares every shard
they have in common **by construction**: shared content collapses to one target
(computed **exactly once**), and a standalone scenario **extends or grows into a
design with zero recomputation** of shared shards. A "scenario" becomes a
**selection** over one shared results tree, not a storage owner.

## What Changes

- **New `ssd_design(...)` collection constructor** — a validated, named collection
  of `ssdsims_scenario` objects (class `ssdsims_design`), mirroring the
  `ssd_scenario_data()` naming convention (names from explicit `name =` arguments
  or derived from the argument expression). Names are **selection labels** for the
  combined results and the derived membership mapping — **not** addressing: they
  do **not** enter target names or storage paths. A design of **one** scenario is
  valid and uniformly shaped; an empty call aborts.
- **New `ssd_design_targets(design, ..., root, upload, cue)` target factory** —
  composes the existing per-scenario target sets into one static-branching
  pipeline, projecting each member to the per-shard content so a shard's target
  **identity** (name *and* command) is a pure function of its content. Members
  that share content thus collapse to a single target (one build — the dedup);
  members with distinct content (different axes/cells) stay distinct. A single
  `tar_make()` runs the whole design under one controller and the keep-going
  `error` policy.
- **Exactly-once and extend-without-recompute.** Each unique shard is built once;
  wrapping a completed standalone scenario in a design, or adding/removing a
  member, builds only the genuinely-new shards and reports every shared shard
  cached and byte-identical (the `path-axis-growth` contract lifted one level).
- **A combined design summary keyed by partition coordinates** — a top-level
  `summary` target unions the per-coordinate compact summaries into
  `<root>/summary.parquet` at the DuckDB level (no R materialise, survivors-union),
  keyed by coordinates (`dataset`, axis values) with **no `scenario` column**.
  Scenario membership is a **derived** selection (which scenarios' selections cover
  each row), never stored on a shard — shared with `cost-analysis-targets`.
- **Common random numbers.** Two members sharing `(seed, primer)` on an
  overlapping task resolve to the **same shard**, computed once. Documented; never
  warned about or forbidden.
- **Scalar-setting divergence is not specially handled.** If two members differ
  only in a scalar setting at a shared cell (e.g. `est_method`), the shard simply
  reflects whatever was computed and is recomputed/overwritten when it changes —
  v2 adds no per-setting addressing for it (decision below). The primary
  comparison dimension, `distset`, is a real **axis** and so produces distinct
  shards that share their common ancestors naturally.

## Capabilities

### New Capabilities
- `scenario-combine-v2`: the **design** — `ssd_design()` (a named scenario
  collection whose names are selection labels), `ssd_design_targets()` composing
  the existing per-scenario targets into one deduplicated pipeline, the
  exactly-once and extend-without-recompute guarantees, and the coordinate-keyed
  combined summary with membership derived.

### Modified Capabilities
<!-- None: the single-scenario factory's addressing is already content-pure and
     unchanged; this change adds the design layer above it. -->

## Impact

- **No new prerequisite change.** Rests on the existing content-pure addressing of
  `ssd_scenario_targets()` and on `distset-hc-axis` (landed). There is no
  `content-addressed-shards` to wait on.
- **Supersedes the original `scenario-combine`** (archived as reference, specs not
  synced): the design *concept* and `ssd_design()` collection are retained; the
  `scenario=<name>` decoupling, `<name>_` prefixes, and `scenario`-column summary
  are replaced by composition over the shared tree and a coordinate-keyed summary.
- **New code:** `ssd_design()` (collection constructor + validation, `R/design.R`);
  `ssd_design_targets()` and the coordinate-keyed combined-summary fan-in
  (`R/targets-runner.R`); a stubbed read-side scenario→selection derivation
  (coordinated with `cost-analysis-targets`).
- **APIs:** two new exports (`ssd_design()`, `ssd_design_targets()`); no breaking
  change to existing exports.
- **Docs:** `GLOSSARY.md` *Design terms* (scenario/design as selections over a
  shared tree), `vignettes/sharded-pipeline.qmd` (a design section), `README.Rmd`,
  `_pkgdown.yml`, `ROADMAP.md`.
- **Tests:** dedup (members sharing `sample`/`fit`/`hc` content build it once);
  extend/grow-without-recompute (standalone → design, all shared shards cached and
  byte-identical; add/remove a member); coordinate-keyed combined summary (no
  `scenario` column, overlapping coordinate appears once, survivors-union); a
  design of one and empty-call validation.

## Dependencies

- **Depends on:** nothing new — the existing content-pure addressing of
  `ssd_scenario_targets()`, plus `distset-hc-axis` (landed; makes `distset` an
  axis so pool comparison rides distinct cells).
- **Coordinates with:** `cost-analysis-targets` (shared derived scenario→selection
  mapping over the coordinate space).
- **Supersedes (archived as reference):** the original `scenario-combine`; and
  `content-addressed-shards`, explored this cycle and found moot.
