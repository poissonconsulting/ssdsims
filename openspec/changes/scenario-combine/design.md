# Design: scenario-combine

## Context

`ssd_scenario_targets()` is a target factory: sourcing `_targets.R` computes the
shard tables and returns one named `format = "file"` target per `partition_by`
path cell per step, plus a `summary` fan-in, all written under the per-layout
`scenario_results_dir(scenario)` root. Two facts block "just concatenate two
factories' output in one `_targets.R`":

- **Target names collide.** A step target's name is `<step>_step_<path cells>`
  (`tar_map()` suffixes from the path-axis values), so two scenarios over the
  same dataset/sim cells mint identical names, and `targets` aborts on
  duplicates. The `summary` target name is a constant.
- **Roots collide.** `scenario_results_dir()` keys on `partition_by` only, so
  two scenarios with the same layout write into the *same* Hive tree, and the
  depth-agnostic readers (`<step>/**/part.parquet`) would union both scenarios'
  shards into each other's results.

Meanwhile the motivating comparisons — `dists`, `est_method`, `nrow_max`,
`ci` — are deliberately *simulation settings*, scenario-wide and absent from
`task_axes()`, so they cannot fan out inside one scenario. Comparing them means
multiple scenarios; today that means multiple stores and multiple `tar_make()`
invocations with no shared controller and no single summary.

## Goals / Non-Goals

**Goals:**

- One `_targets.R`, one store, one `tar_make()` for N scenarios — sharing the
  `crew` controller and the keep-going `error` policy across all shards.
- Per-scenario results byte-identical to running each scenario alone (combining
  changes *addressing* — names and roots — never `(seed, primer)`).
- A combined cross-scenario compact summary with a `scenario` identity column,
  with the same DuckDB-level no-R-materialise and survivors-union properties as
  `ssd_summarise()`.
- The single-scenario `ssd_scenario_targets()` public contract is unchanged.

**Non-Goals:**

- **Cross-scenario shard dedup.** Two scenarios whose task sets overlap (same
  seed, same axes) recompute the overlap under their own roots. Sharing shards
  across scenario trees would couple their invalidation and break the
  "scenario = self-contained results tree" model; if a study needs shared
  draws it should widen one scenario's axes instead.
- **A combined `summary-samples.parquet`.** Retained bootstrap draws stay
  per-scenario (`<root>/scenario=<name>/.../summary-samples.parquet`); the
  cross-scenario product is the compact estimate table.
- **A `name` field on the scenario object.** Names live on the collection
  (the `ssd_data()` pattern), not the scenario — the scenario's declarative
  identity (and every hash derived from it) is untouched.
- The legacy `ssd_run_scenario()` path, the manifest, and replay tooling.

## Decisions

### Decision: compose per-scenario factories, not a merged scenario

`ssd_scenarios_targets()` loops the named scenarios and emits, for each, the
*same* target set the single-scenario factory builds — prefixed and re-rooted —
then appends one combined summary target. No merged "multi-scenario" object
exists; each scenario keeps its own seed, grids, `partition_by`, layout hash,
and results tree.

*Alternative considered:* a mega-scenario whose settings become axes — rejected;
`dists`/`est_method`/`nrow_max`/`ci` are scenario-wide **by design** (the
`dists-simulation-setting` / `est-method-setting` / `scalar-ci-flag` family
moved them off the axes), and reversing that for combination would re-litigate
landed decisions and change task identity. *Alternative considered:* a loop of
separate `_targets.R` stores driven by a script — rejected; that is the status
quo being replaced (no shared scheduling, no single store, no combined
summary, N invocations).

### Decision: `ssd_scenarios(...)` owns naming, mirroring `ssd_data()`

A collection constructor returns a classed `ssdsims_scenarios` named list.
Names come from explicit argument names or are derived from the argument
expression (the established `ssd_data()` derivation), and are validated:
unique, non-empty, non-`NA`, and shaped to serve as both a target-name prefix
and a Hive directory level (start with a letter; letters, digits, `_` only —
the conservative intersection of valid R symbol fragments and path-safe
strings). Each element must be an `ssdsims_scenario`.
`ssd_scenarios_targets()` accepts **only** this collection — consistent with
the package direction (`scenario-input-types` removes the bare convenience
forms from `ssd_define_scenario()`; collections own naming and validation).

*Alternative considered:* accept a bare named list — rejected for two entry
points to the same validation and inconsistency with where the package is
heading.

### Decision: thread an internal target-name prefix through the existing factory

The single-scenario factory's internals (`step_map()`, `shard_cell_names()`,
the `summary` target name) gain an internal `prefix` parameter (default `""`),
prepended to every minted target name: `<prefix>sample_step_<cells>`,
`<prefix>upload_<step>_<cells>`, `<prefix>summary`. `ssd_scenario_targets()`
passes the empty prefix, so its emitted target definitions are unchanged
byte-for-byte; `ssd_scenarios_targets()` passes `<name>_`. The per-child
upstream edges and the summary's `edge_block()` resolve through
`shard_cell_names()`, so prefixed names flow through one source of truth.

*Alternative considered:* post-hoc renaming of the returned target objects —
rejected; the per-child edge blocks splice target names into *commands*, so
renaming after assembly would desynchronise the dependency graph from the
names.

### Decision: per-scenario roots at `<root>/scenario=<name>/layout=<hash>`

Each scenario's targets write under
`scenario_results_dir(scenario, root = file.path(root, paste0("scenario=", name)))`.
The `scenario=` Hive-style level keeps the tree self-describing and isolates
scenarios *before* the layout key, so two scenarios sharing a `partition_by`
never mix shards, and each scenario's subtree is byte-identical in layout to a
standalone run under that root. A scenario rename is addressing-only (a fresh
subtree; the old one is abandoned, the standard layout-change behaviour).

### Decision: the combined summary unions per-scenario compact summaries

A top-level `summary` target (the only unprefixed name the multi factory
mints) names every per-scenario `<name>_summary` target for ordering and
invalidation, then calls a new `ssd_summarise_scenarios(summaries, path)` with
the named per-scenario *compact* summary paths (computed from the roots — when
a scenario retains draws its summary target returns two paths; only the
compact one feeds the union). Each file is read lazily via `duckplyr`, tagged
with a literal `scenario` column, and `union_all`-ed straight to
`<root>/summary.parquet` inside DuckDB — never collecting into R. Files that
did not land are skipped (the per-scenario summaries carry the pipeline's
keep-going policy), so the combined summary unions the surviving scenarios,
mirroring `ssd_summarise()`'s §6.2 property. Schemas align across scenarios
because the compact hc summary's column set is fixed (bootstrap-only columns
ride as `NA` under `ci = FALSE`).

### Decision: upload destinations get a per-scenario prefix extension

With a non-`NULL` `upload`, each scenario's paired `upload_<step>` targets ship
under the destination's blob prefix extended by `scenario=<name>` (an internal
"extend prefix" helper on the `ssdsims_upload` object; `ssd_upload_dryrun()`
passes through unchanged). The remote layout then mirrors the local tree and
`ssd_open_uploaded()` reads a single scenario back by pointing at the extended
prefix. The factory still performs no network I/O.

### Decision: shared seeds are common random numbers, documented not forbidden

Two scenarios with the same `seed` and overlapping task identities share
`(seed, primer)` pairs — their overlapping draws are *identical*. That is the
classic variance-reduction pairing for setting comparisons (the primer hashes
axes only, so scenarios differing in a simulation setting are exactly paired),
so the factory neither warns nor validates seed distinctness; the behaviour is
documented on `ssd_scenarios_targets()`. Distinct seeds give independent
streams as always.

## Risks / Trade-offs

- **Constructed target names could still collide** (a scenario name plus a
  cell suffix echoing another scenario's prefix, e.g. names `a` / `a_sample`)
  → the name validation forbids the pathological shapes we can see, and
  `targets` itself aborts on duplicate target names at sourcing time — a loud,
  immediate backstop, never a silent merge. Documented on `ssd_scenarios()`.
- **Sourcing cost scales with the union of shard counts** (every scenario's
  task tables are computed when `_targets.R` is sourced, on every worker) →
  already the single-scenario model and the §1 "task set is a pure function of
  the scenario" contract; tiny tables, no new mechanism.
- **A duplicated scenario object under two names doubles compute** (no
  cross-scenario dedup, by Non-Goal) → identical results under both names;
  cheap to detect later if it bites — out of scope here.
- **`union_all` schema drift across scenarios** (a future hc column change
  could misalign older trees) → the combined summary unions *this pipeline's*
  fresh per-scenario summaries (same package version writes all of them), not
  arbitrary historical files.

## Migration Plan

Additive only: two new exports, no signature or behaviour change to existing
functions, no re-baselining (per-task results are unchanged). Nothing to roll
back beyond reverting the commit.

## Open Questions

- Whether to ship a multi-scenario `inst/targets-templates/` template or fold
  a multi-scenario section into the existing template and vignette — resolved
  during implementation (leaning: vignette section + extend the existing
  template's comments; a third template is more surface to keep in sync).
