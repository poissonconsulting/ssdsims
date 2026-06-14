# Proposal: scenario-combine

> **ARCHIVED — superseded, not implemented (specs not synced).** This change's
> design layer is rebuilt on content-pure addressing as `scenario-combine-v2`,
> which composes the content-addressed targets from `content-addressed-shards`
> instead of giving each member its own `scenario=<name>` tree and `<name>_`
> prefix. The `ssd_design()` collection and the design *concept* here carry over
> to v2; the per-scenario decoupling and the `scenario`-column summary do not.
> Retained as reference for the naming/validation design and the decision trail.

## Why

A simulation study routinely compares **simulation settings** — `dists`,
`est_method`, `nrow_max`, `ci` — which are scenario-wide by design (not
cross-join axes), so the comparison *cannot* be expressed inside one
`ssd_scenario`: a single scenario is one **regular** grid. The comparison is a
**non-regular** design — a union of regular sub-grids. Today each scenario needs
its own `_targets.R`, its own store, and its own `tar_make()`: no shared
scheduling across a cluster controller, no single summary, and N pipelines to
babysit. The ROADMAP books this as ❗️ [scenario-combine]: *provide a convenient
way to run multiple `ssd_scenario` objects as a single targets pipeline.*

This change introduces the **design** as that unit (DoE sense: the set of
conditions to run): a named set of scenarios run as one pipeline. It sits in the
hierarchy **scenario ⊂ design ⊂ study** the `GLOSSARY.md` *Design terms* section
defines — a scenario is one regular grid, a design unions scenarios into one
pipeline run, and a *study* (the longitudinal aggregate across infrastructure,
time, and software versions) is reserved as a future read-side concept, not
built here.

## What Changes

- **New `ssd_design(...)` collection constructor** — a validated, named
  collection of `ssdsims_scenario` objects (class `ssdsims_design`), mirroring
  the `ssd_data()` naming convention: names from explicit `name =` arguments or
  derived from the argument expression. Each name is a **scenario name** within
  the design; names must be unique, non-empty, and safe for both target names
  and result paths (they become the per-scenario target-name prefix and a
  `scenario=<name>` directory level). A design of **one** scenario is valid and
  uniformly shaped (no special-casing) — the recommended starting point for
  studies that may grow.
- **Design growth caches existing members**: adding a scenario to an
  already-run design builds only the new member's targets plus the combined
  summary; every existing member's targets are reported cached and their shards
  stay byte-identical, because member addressing is independent of the design's
  other members (the path-axis-growth contract, one level up). Promoting a flat
  `ssd_scenario_targets()` run into a design is documented as safe but
  recomputing (addressing changes; `(seed, primer)` does not).
- **New `ssd_design_targets(design, ..., root, upload, cue)` target factory** —
  the multi-scenario sibling of `ssd_scenario_targets()`. For each named
  scenario it emits the full single-scenario target set with every target name
  prefixed `<name>_` (no collisions), written under a per-scenario, per-layout
  root `<root>/scenario=<name>/layout=<hash>` (no shard mixing, even when two
  scenarios share a `partition_by`). A single `tar_make()` then runs the whole
  design's shards under one scheduler/controller.
- **A combined design summary**: a top-level `summary` target that names every
  per-scenario summary target and unions the per-scenario compact summaries into
  `<root>/summary.parquet` with a `scenario` identity column — performed at the
  DuckDB level (the same no-R-materialise guarantee as `ssd_summarise()`). This
  is the design's results table.
- **Per-scenario upload separation**: with a non-`NULL` `upload`, each
  scenario's shards ship under a `scenario=<name>` extension of the
  destination's blob prefix, so the remote layout mirrors the local one.
- **Byte identity is preserved**: a scenario's per-task results in the design
  pipeline are byte-identical to running it alone through
  `ssd_scenario_targets()` — combining changes addressing (names, roots) only,
  never `(seed, primer)`. Scenarios sharing a `seed` deliberately share per-task
  streams on overlapping task identities (common random numbers for paired
  comparisons); this is documented, not forbidden.
- `ssd_scenario_targets()`'s public contract is **unchanged** — the prefix and
  root threading is an internal refactor the single-scenario factory composes
  with.

## Capabilities

### New Capabilities
- `scenario-combine`: the **design** — a named scenario collection
  (`ssd_design()`), the design target factory (`ssd_design_targets()`) with
  per-scenario target-name prefixes and `scenario=<name>` result roots, the
  combined design summary with its `scenario` identity column, and the
  per-scenario upload prefixing.

### Modified Capabilities
<!-- None: `ssd_scenario_targets()`'s requirements are untouched; the design
     factory composes the existing single-scenario machinery internally. -->

## Impact

- **New code**: `ssd_design()` (collection constructor + validation,
  `R/design.R`); `ssd_design_targets()` and the combined-summary fan-in
  (`R/targets-runner.R` or a sibling file).
- **Internal refactor**: thread a target-name prefix through the
  `ssd_scenario_targets()` internals (`step_map()`, `shard_cell_names()`, the
  `summary` target name) with the empty prefix reproducing today's names
  byte-for-byte.
- **APIs**: two new exports (`ssd_design()`, `ssd_design_targets()`); no
  breaking change to existing exports.
- **Docs**: README / `vignettes/sharded-pipeline.qmd` (a design section
  steering growable studies to start as a design of one, with the
  flat-to-design promotion documented as safe but recomputing),
  `_pkgdown.yml`, `GLOSSARY.md` (the *Design terms* `scenario`/`design`/`study`
  entries — landed in this change), `ROADMAP.md` (move the entry), regenerated
  `man/`; optionally a design `inst/targets-templates/` template.
- **Tests**: integration test running a two-scenario design in one pipeline
  (byte-identity against the single-scenario runs; combined summary content);
  a design-growth test (design of one grown by a member: existing shards
  cached and byte-identical, only the new member and combined summary build);
  validation tests for the collection (duplicate/empty/unsafe names; a design
  of one is valid, an empty call aborts); upload-prefix shape under
  `ssd_upload_dryrun()`.
- **Dependencies**: none — independent of the in-flight
  `scenario-input-types` (different functions; only GLOSSARY/vignette prose
  could brush against it).
