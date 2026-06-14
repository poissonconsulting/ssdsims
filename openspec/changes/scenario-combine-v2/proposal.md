# Proposal: scenario-combine-v2

## Why

A simulation study routinely compares **simulation settings** (`est_method`,
`ci`, `nrow_max`) or distribution-set pools (`distset`) across otherwise-shared
ground. The ROADMAP books this as ‼️ [scenario-combine]: *provide a convenient
way to run multiple `ssd_scenario` objects as a single targets pipeline.* The
original `scenario-combine` proposed to do this by giving each member scenario its
own `scenario=<name>` results tree and `<name>_`-prefixed targets — making a
design **N independent pipelines that share a `tar_make()`**, recomputing the
shared prefix once per member. That per-scenario decoupling is now unnecessary and
is **archived as reference**: `content-addressed-shards` (this change's
prerequisite) made the single-scenario factory address shards by **content** — a
pure function of partition cells plus per-step scalar discriminators, with no
scenario identity — so identical content already resolves to one target name and
path.

This **v2** builds the design layer *on that foundation*. Because addressing is
content-pure, running several scenarios in one pipeline shares every shard they
have in common **by construction**: identical content collapses to one target, so
it is computed **exactly once**, and a standalone scenario **extends or grows into
a design with zero recomputation** of shared shards. A "scenario" becomes a
**selection** over one shared results tree, not a storage owner.

## What Changes

- **New `ssd_design(...)` collection constructor** — a validated, named collection
  of `ssdsims_scenario` objects (class `ssdsims_design`), mirroring the
  `ssd_scenario_data()` naming convention (names from explicit `name =` arguments
  or derived from the argument expression). Names are **selection labels** for the
  combined results and the derived membership mapping — **not** addressing: they
  do **not** enter target names or storage paths (those are content-pure, per
  `content-addressed-shards`). A design of **one** scenario is valid and uniformly
  shaped; an empty call aborts.
- **New `ssd_design_targets(design, ..., root, upload, cue)` target factory** —
  composes the **content-addressed** targets each member scenario emits into one
  static-branching pipeline. Shards shared across members collapse to a single
  target (identical content ⇒ identical name ⇒ one build — the dedup); distinct
  content stays distinct. A single `tar_make()` runs the whole design under one
  controller and the keep-going `error` policy.
- **Exactly-once and extend-without-recompute.** Each unique shard is built once;
  wrapping a completed standalone scenario in a design, or adding/removing a
  member, builds only the genuinely-new shards and reports every shared shard
  cached and byte-identical (the `path-axis-growth` contract lifted one level).
- **A combined design summary keyed by partition coordinates** — a top-level
  `summary` target unions the per-coordinate compact summaries into
  `<root>/summary.parquet` at the DuckDB level (no R materialise, survivors-union),
  keyed by coordinates (`dataset`, axis values, active discriminators) with **no
  `scenario` column**. Scenario membership is a **derived** selection (which
  scenarios' coordinate-selections cover each row), never stored on a shard —
  shared with `cost-analysis-targets`.
- **Common random numbers, strengthened.** Two members sharing `(seed, primer)` on
  an overlapping task do not merely pair their draws — they resolve to the **same
  shard**, computed once. Documented; never warned about or forbidden.

## Capabilities

### New Capabilities
- `scenario-combine-v2`: the **design** — `ssd_design()` (a named scenario
  collection whose names are selection labels), `ssd_design_targets()` composing
  content-addressed targets into one deduplicated pipeline, the exactly-once and
  extend-without-recompute guarantees, and the coordinate-keyed combined summary
  with membership derived.

### Modified Capabilities
<!-- None: the addressing the design composes on is owned by
     `content-addressed-shards`; this change adds the design layer above it. -->

## Impact

- **Depends on `content-addressed-shards`** (the content-pure addressing it
  composes) and, transitively, `distset-hc-axis`. Chain: `distset-hc-axis` →
  `content-addressed-shards` → `scenario-combine-v2`.
- **Supersedes the original `scenario-combine`** (archived as reference, specs not
  synced): the design *concept* and `ssd_design()` collection are retained; the
  `scenario=<name>` decoupling, `<name>_` prefixes, and `scenario`-column summary
  are replaced by content-addressing and a coordinate-keyed summary.
- **New code:** `ssd_design()` (collection constructor + validation, `R/design.R`);
  `ssd_design_targets()` and the coordinate-keyed combined-summary fan-in
  (`R/targets-runner.R`); a stubbed read-side scenario→selection derivation
  (coordinated with `cost-analysis-targets`).
- **APIs:** two new exports (`ssd_design()`, `ssd_design_targets()`); no breaking
  change to existing exports.
- **Docs:** `GLOSSARY.md` *Design terms* (scenario/design as selections over a
  shared tree), `vignettes/sharded-pipeline.qmd` (a design section), `README.Rmd`,
  `_pkgdown.yml`, `ROADMAP.md`.
- **Tests:** dedup (members sharing `sample`/`fit`/`draw` content build it once);
  extend/grow-without-recompute (standalone → design, all shared shards cached and
  byte-identical; add/remove a member); coordinate-keyed combined summary (no
  `scenario` column, overlapping coordinate appears once, survivors-union); a
  design of one and empty-call validation.

## Dependencies

- **Depends on:** `content-addressed-shards` (content-pure addressing + the `hc`
  `draw`/`summarise` split) and, transitively, `distset-hc-axis`.
- **Coordinates with:** `cost-analysis-targets` (shared derived scenario→selection
  mapping; the other consumer of content-addressed shards).
- **Supersedes (archived as reference):** the original `scenario-combine`.
