# Proposal: scenario-combine

## Why

A simulation study routinely compares **simulation settings** — `dists`,
`est_method`, `nrow_max`, `ci` — which are scenario-wide by design (not
cross-join axes), so the comparison *cannot* be expressed inside one
`ssd_scenario`. Today each configuration needs its own `_targets.R`, its own
store, and its own `tar_make()`: no shared scheduling across a cluster
controller, no single summary, and N pipelines to babysit. The ROADMAP books
this as ❗️ [scenario-combine]: *provide a convenient way to run multiple
`ssd_scenario` objects as a single targets pipeline.*

## What Changes

- **New `ssd_scenarios(...)` collection constructor** — a validated, named
  collection of `ssdsims_scenario` objects (class `ssdsims_scenarios`),
  mirroring the `ssd_data()` naming convention: names from explicit `name =`
  arguments or derived from the argument expression. Names must be unique,
  non-empty, and safe for both target names and result paths (they become the
  per-scenario target-name prefix and a `scenario=<name>` directory level).
- **New `ssd_scenarios_targets(scenarios, ..., root, upload, cue)` target
  factory** — the multi-scenario sibling of `ssd_scenario_targets()`. For each
  named scenario it emits the full single-scenario target set with every target
  name prefixed `<name>_` (no collisions), written under a per-scenario,
  per-layout root `<root>/scenario=<name>/layout=<hash>` (no shard mixing, even
  when two scenarios share a `partition_by`). A single `tar_make()` then runs
  all scenarios' shards under one scheduler/controller.
- **A combined cross-scenario summary**: a top-level `summary` target that
  names every per-scenario summary target and unions the per-scenario compact
  summaries into `<root>/summary.parquet` with a `scenario` identity column —
  performed at the DuckDB level (the same no-R-materialise guarantee as
  `ssd_summarise()`).
- **Per-scenario upload separation**: with a non-`NULL` `upload`, each
  scenario's shards ship under a `scenario=<name>` extension of the
  destination's blob prefix, so the remote layout mirrors the local one.
- **Byte identity is preserved**: a scenario's per-task results in the combined
  pipeline are byte-identical to running it alone through
  `ssd_scenario_targets()` — combining changes addressing (names, roots) only,
  never `(seed, primer)`. Scenarios sharing a `seed` deliberately share
  per-task streams on overlapping task identities (common random numbers for
  paired comparisons); this is documented, not forbidden.
- `ssd_scenario_targets()`'s public contract is **unchanged** — the prefix and
  root threading is an internal refactor the single-scenario factory composes
  with.

## Capabilities

### New Capabilities
- `scenario-combine`: the named scenario collection (`ssd_scenarios()`), the
  multi-scenario target factory (`ssd_scenarios_targets()`) with per-scenario
  target-name prefixes and `scenario=<name>` result roots, the combined
  cross-scenario summary with its `scenario` identity column, and the
  per-scenario upload prefixing.

### Modified Capabilities
<!-- None: `ssd_scenario_targets()`'s requirements are untouched; the combined
     factory composes the existing single-scenario machinery internally. -->

## Impact

- **New code**: `ssd_scenarios()` (collection constructor + validation,
  `R/scenarios.R`); `ssd_scenarios_targets()` and the combined-summary fan-in
  (`R/targets-runner.R` or a sibling file).
- **Internal refactor**: thread a target-name prefix through the
  `ssd_scenario_targets()` internals (`step_map()`, `shard_cell_names()`, the
  `summary` target name) with the empty prefix reproducing today's names
  byte-for-byte.
- **APIs**: two new exports (`ssd_scenarios()`, `ssd_scenarios_targets()`);
  no breaking change to existing exports.
- **Docs**: README / `vignettes/sharded-pipeline.qmd` (a multi-scenario
  section), `_pkgdown.yml`, `GLOSSARY.md` (scenario *name*), `ROADMAP.md`
  (move the entry), regenerated `man/`; optionally a multi-scenario
  `inst/targets-templates/` template.
- **Tests**: integration test running two tiny scenarios in one pipeline
  (byte-identity against the single-scenario runs; combined summary content);
  validation tests for the collection (duplicate/empty/unsafe names);
  upload-prefix shape under `ssd_upload_dryrun()`.
- **Dependencies**: none — independent of the in-flight
  `scenario-input-types` (different functions; only GLOSSARY/vignette prose
  could brush against it).
