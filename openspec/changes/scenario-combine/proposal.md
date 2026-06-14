# Proposal: scenario-combine

## Why

A simulation study routinely compares **simulation settings** — `dists`,
`est_method`, `proportion`, `nrow_max`, `ci` — which are scenario-wide by design
(not cross-join axes), so the comparison *cannot* be expressed inside one
`ssd_scenario`: a single scenario is one **regular** grid. The comparison is a
**non-regular** design — a union of regular sub-grids. Today each scenario needs
its own `_targets.R`, its own store, and its own `tar_make()`: no shared
scheduling across a cluster controller, no single summary, and N pipelines to
babysit. The ROADMAP books this as ❗️ [scenario-combine]: *provide a convenient
way to run multiple `ssd_scenario` objects as a single targets pipeline.*

This change introduces the **design** as that unit (DoE sense: the set of
conditions to run): a named set of scenarios run as one pipeline, in the
hierarchy **scenario ⊂ design ⊂ study** the `GLOSSARY.md` *Design terms* section
defines. Crucially — and this is the substantive shift from the first cut of
this proposal — the design **shares shards across scenarios** instead of
isolating them: two scenarios that compare a downstream setting (e.g. `dists` or
`est_method`) reuse their common upstream shards (the same `sample` draws, often
the same `fit`s) rather than recomputing them under a private tree. Combining is
a variance-reduction and compute-saving operation, not just a scheduling
convenience.

## What Changes

- **New `ssd_design(...)` collection constructor** — a validated, named
  collection of `ssdsims_scenario` objects (class `ssdsims_design`), mirroring
  the `ssd_data()` naming convention. Each name is a **scenario name** within the
  design; names must be unique, non-empty, and safe (they are the `scenario`
  identity-column value and the per-scenario summary target-name suffix). Names
  enter addressing **only** at the summary layer — never task identity, the
  primer, or a shard path. A design of **one** scenario is valid and uniformly
  shaped.
- **Content-keyed shard addressing replaces scenario-keyed addressing.** Shards
  are no longer written under a `scenario=<name>` prefix, and target names no
  longer carry a `<name>_` prefix. Instead each step's shards are addressed by a
  **per-step cumulative content key** `sig=<hash>` — a digest of the `seed` plus
  exactly the scenario fields that step's runner consumes (the existing
  `scenario_step_slice()` projection), cumulated down the DAG
  (`sample_sig → fit_sig → hc_sig`). Two scenarios whose step content is
  byte-identical mint **one** target at **one** path and the shard is computed
  **once**; scenarios that differ in a step's content get distinct sigs and never
  mix shards.
- **Cross-scenario shard dedup becomes a goal (was a non-goal).** Because
  `nrow_max` is a uniform draw-size guard (not a comparison axis) and the per-task
  primer is seed-anchored, every same-seed scenario in a design shares **all
  `sample` shards**; scenarios that agree on `dists` additionally share their
  `fit` (and `hc`) shards. `dists` is the only setting that forks an upstream
  shard.
- **hc readout settings are aggregated so they do not fork the hc shard.** The
  seed-free hc readouts are computed as a maximal set in one shared `hc` shard and
  each scenario's summary filters its slice: **`union()` over `proportion` and
  `est_method`**, **`any()` over `ci` and `samples`**. So `hc_sig` keys only the
  seed-dependent **draw-shapers** (the parent `fit` lineage, i.e. `dists`, plus
  the `nboot`/`ci_method`/`parametric`/`distset` cells). This reuses
  `ssdtools::ssd_hc()`'s existing vectorized `proportion`/`est_method` and the
  package's own `hc_collapse_est_methods()` (one bootstrap, analytical `est` per
  method) — **no ssdtools refactor**.
- **New `ssd_design_targets(design, ..., root, upload, cue)` target factory** —
  emits the **content-keyed union** of every scenario's shard targets
  (de-duplicated by name) under a per-step, sig-addressed root
  `<root>/<step>/sig=<hash>/<cells>`, plus one combined summary. A single
  `tar_make()` runs the whole design under one scheduler/controller.
- **Per-scenario summaries that filter shared shards, unioned into one table.**
  Each scenario's `summary_<name>` target reads the shared shards and filters to
  that scenario's readout/cell slice; the top-level `summary` target unions them
  into `<root>/summary.parquet` with a `scenario` identity column, at the DuckDB
  level (the no-R-materialise guarantee of `ssd_summarise()`).
- **Upload mirrors the content-keyed tree.** With a non-`NULL` `upload`, shards
  ship under the same sig-addressed paths (so a shared shard uploads **once**);
  read-back resolves a scenario's sigs. The per-scenario `scenario=<name>` upload
  prefix is removed.
- **Byte identity is preserved.** A scenario's per-task results are byte-identical
  to running it alone — combining changes addressing (names, roots) only, never
  `(seed, primer)`. Shard sharing is *licensed* by that byte-identity: a shared
  shard is the result both scenarios would have computed.
- `ssd_scenario_targets()`'s public contract is **unchanged** — it keeps its
  `layout=<hash(partition_by)>` addressing; the design factory's sig addressing is
  new code. Promoting a flat run into a design is documented as safe but
  recomputing (addressing changes; `(seed, primer)` does not).

## Capabilities

### New Capabilities
- `scenario-combine`: the **design** — `ssd_design()`; per-step cumulative
  content-key (`sig=`) shard addressing with cross-scenario dedup;
  `ssd_design_targets()` emitting the content-keyed target union; the hc readout
  aggregation (`union` over `proportion`/`est_method`, `any` over `ci`/`samples`)
  feeding shared `hc` shards; per-scenario summary filtering and the combined
  design summary with its `scenario` identity column; sig-mirrored upload.

### Modified Capabilities
<!-- None: `ssd_scenario_targets()`'s requirements (and the `task-shards` /
     `shard-runner` per-task contracts) are untouched. The design factory
     composes the existing single-scenario machinery, supplying aggregated
     step slices and sig-addressed roots; the per-shard runners and `ssd_hc()`
     usage are unchanged. -->

## Impact

- **New code**: `ssd_design()` (collection constructor + validation,
  `R/design.R`); `ssd_design_targets()`, the content-keyed sig helpers, the
  aggregated hc step-slice, the per-scenario summary filter, and the combined
  summary fan-in (`R/targets-runner.R` or a sibling file).
- **Internal refactor**: a sig-addressed results-dir helper (the design sibling
  of `scenario_results_dir()`); a `sig` parameter threaded through the design
  factory's target-name and root construction (the empty/standalone path keeps
  `ssd_scenario_targets()` byte-for-byte).
- **APIs**: two new exports (`ssd_design()`, `ssd_design_targets()`); no breaking
  change to existing exports. **No ssdtools change.**
- **Docs**: README / `vignettes/sharded-pipeline.qmd` (a design section covering
  shard sharing, the readout aggregation, and the growth contract), `_pkgdown.yml`,
  `GLOSSARY.md` (*Design terms*), `ROADMAP.md`, regenerated `man/`.
- **Tests**: a two-scenario design sharing `sample`/`fit` shards (one target,
  one Parquet, byte-identical to the standalone per-task results); a `dists`
  comparison forking `fit` while sharing `sample`; an `est_method`/`proportion`/
  `ci`/`samples` comparison sharing **all** shards and filtering per scenario;
  combined-summary content + `scenario` tag; design growth (adding a member adds
  only its non-shared targets; widening the hc readout union re-sigs only the
  `hc` shards); upload-shape under `ssd_upload_dryrun()`; sig-completeness
  (each off-axis setting that *does* change bytes forks the sig).
- **Dependencies**: none — independent of in-flight changes (only GLOSSARY/
  vignette prose could brush against `scenario-input-types`).
