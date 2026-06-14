# Design: scenario-combine

## Context

`ssd_scenario_targets()` is a target factory: sourcing `_targets.R` computes the
shard tables and returns one named `format = "file"` target per `partition_by`
path cell per step, plus a `summary` fan-in, all written under the per-layout
`scenario_results_dir(scenario)` root (`<root>/layout=<hash(partition_by)>`). The
per-task RNG is anchored by the scenario `seed` plus a per-task **primer**
(`task_primer()` over `task_axes(step)`); the primer hashes **task axes only**,
deliberately excluding the scenario-wide simulation settings. Two facts shape the
design factory:

- **Target names and roots are content-blind.** A step target's name is
  `<step>_step_<path cells>` and its root keys on `partition_by` only. Neither
  encodes the off-axis settings (`dists`, `est_method`, `proportion`, `ci`,
  `samples`, `nrow_max`) that nonetheless determine a shard's bytes. Within one
  scenario these are fixed, so it is fine; *across* scenarios it is not.
- **A shard's bytes are a pure function of `(seed, primer, the off-axis settings
  that step's runner reads)`.** The package already names that off-axis read-set:
  `scenario_step_slice(scenario, step)` (`R/targets-runner.R`) is "the minimal
  `ssdsims_scenario`-classed sub-object the step's runner consumes … a
  deterministic, hashable projection … two computations are byte-identical and
  produce the same dependency hash." It was built for *per-step cache
  independence*; the same projection is exactly what licenses *cross-scenario
  sharing*.

The first cut of this change isolated scenarios with a `scenario=<name>` root
prefix and a `<name>_` target-name prefix — a blunt, whole-scenario content key
that guarantees correctness by never sharing anything. That recomputes shards two
scenarios provably share (the `sample` draws under common random numbers; the
`fit`s when only a downstream setting differs). This revision replaces
scenario-keyed addressing with **per-step content-keyed addressing**, turning
cross-scenario dedup from a non-goal into the point.

## Goals / Non-Goals

**Goals:**

- One `_targets.R`, one store, one `tar_make()` for a whole design.
- **Cross-scenario shard sharing:** a shard whose `(seed, step content)` is
  byte-identical across scenarios is minted as one target, written once, and read
  by every scenario that needs it. Sharing is *licensed* by byte-identity.
- Per-scenario results byte-identical to running each scenario alone (combining
  changes *addressing* — names and roots — never `(seed, primer)`).
- A combined design summary (the compact estimate table) with a `scenario`
  identity column, each scenario's rows filtered from the shared shards, unioned
  at the DuckDB level (the `ssd_summarise()` no-R-materialise + survivors-union
  properties).
- The single-scenario `ssd_scenario_targets()` public contract is unchanged.
- **No ssdtools refactor** — the hc readout aggregation rides on
  `ssdtools::ssd_hc()`'s existing vectorized `proportion`/`est_method` and the
  package's `hc_collapse_est_methods()`.

**Non-Goals:**

- **The study level.** A *study* is a read-side union across design-runs
  (infrastructure, time, software versions); deferred (a future `study` column).
- **Splitting the hc step into a draws shard + a readout layer.** Growth-robust
  readout axes (a new `proportion` Hive level over a shared bootstrap-draws
  shard) are explicitly *not* built here; the readouts are union/`any`-aggregated
  into the existing single `hc` shard instead (see the readout decision).
- **Content-addressing the single-scenario factory.** `ssd_scenario_targets()`
  keeps `layout=<hash(partition_by)>`; only the design factory is sig-addressed.
  Promotion flat→design therefore recomputes (addressing differs; results do
  not).
- **A `name` field on the scenario object.** Names live on the design collection;
  the scenario's declarative identity (and every hash derived from it) is
  untouched.
- The legacy `ssd_run_scenario()` path, the manifest, and replay tooling.

## Decisions

### Decision: per-step cumulative content keys (`sig=`) address shards

Each step's shards are written under `<root>/<step>/sig=<stepsig>/<cells>/part.parquet`
and minted as targets named `<step>_<stepsig>_<cells>`, where `stepsig` is a
short digest cumulated down the DAG:

```
sample_sig = hash(seed, scenario_step_slice(scenario, "sample"))
fit_sig    = hash(sample_sig, scenario_step_slice(scenario, "fit"))
hc_sig     = hash(fit_sig,    hc_draw_slice(scenario))
```

- **`seed` is hashed explicitly.** `scenario_step_slice()` omits `seed` (it rides
  in each shard's `tasks` list-column), so a naïve slice hash would let two
  *different-seed* scenarios collide. Hashing `seed` into `sample_sig` (whence it
  cumulates) is the correctness anchor.
- **Cumulation propagates upstream identity.** `fit_sig` depends on `sample_sig`
  so a different `sample` (e.g. a different seed, or a different `nrow_max`)
  forks the `fit`. This rides naturally on the parent-shard *path reference*: a
  `fit` shard reads its parent `sample` by a path that already carries
  `sample_sig`, so the dependency edge and the cache invalidation are one and the
  same.
- **The cells are unchanged** — they remain `task_axes(step)` path values
  (`partition_by`), so identical-cell tasks across scenarios coincide naturally;
  `sig` is the *off-axis* complement that the old `scenario=<name>` prefix used to
  stand in for.

*Alternative considered:* keep `scenario=<name>` (whole-scenario key) — rejected;
it recomputes provably-shared shards. *Alternative considered:* a global
content-address (hash the shard's full input) with no step structure — rejected;
the per-step slice is already the package's source of truth and keeps the tree
legible (`<step>/sig=…`).

### Decision: cross-scenario sharing is the assembly model

`ssd_design_targets()` loops the design's scenarios, builds each one's
sig-addressed shard targets, and returns their **union de-duplicated by target
name**. Two scenarios that produce the same `(step, sig, cells)` produce a
*byte-identical target definition* — same name, same command, same `format =
"file"` path — so keeping one is sound. This requires each shard target's command
to close over the **step slice (and design-wide aggregates)**, not the whole
scenario object, so two scenarios' shared shards are literally identical
definitions (the `scenario_step_slice()` contract already guarantees this for the
per-task body).

What forks what, after this and the readout decision below:

```
  comparison        sample   fit    hc     shared
  ────────────────  ──────   ────   ────   ──────────────────────
  dists             share    FORK   fork   sample
  est_method        share    share  share  sample + fit + hc   (readout union)
  proportion        share    share  share  sample + fit + hc   (readout union)
  ci / samples      share    share  share  sample + fit + hc   (readout any)
  nrow / partition  — within-scenario axes —
  nrow_max          — uniform draw-size guard, not a comparison axis —
  seed (distinct)   FORK     fork   fork   nothing (independent streams)
```

`dists` is the only setting that forks an *upstream* shard; everything else
either shares fully or rides as a within-scenario axis.

*Alternative considered:* concatenate per-scenario prefixed target sets (the
first cut) — rejected; no sharing, and prefixing actively *defeats* it (two
targets racing on one path).

### Decision: hc readouts are aggregated, not sig-forked (`union` / `any`)

The hc inputs split into **seed-dependent draw-shapers** (which change the
bootstrap draws — the parent `fit`, `nboot`, `parametric`, and the `distset`
subset) and **seed-free readouts** (which read out of the *same* draws —
`est_method`, `proportion`, `ci`, `samples`). Only draw-shapers belong in
`hc_sig`. The readouts are computed once as a **maximal set** in the shared `hc`
shard, and each scenario's summary filters its slice:

- **`union()` over `proportion` and `est_method`.** `ssd_hc()` already vectorizes
  both over a single bootstrap (one bootstrap, analytical `est` per method via
  `hc_collapse_est_methods()`), so the shard emits the design-wide union of
  `(proportion × est_method)` rows; a scenario keeps its subset.
- **`any()` over `ci` and `samples`.** `est` is byte-identical across `ci`
  (guaranteed by `hc_collapse_est_methods()`), and `ci = TRUE` / `samples = TRUE`
  are strict supersets (they add `se`/`lcl`/`ucl` and the retained draws). The
  shard computes `ci = any(ci)`, `samples = any(samples)`; a `ci = FALSE`
  consumer reads only `est` (from any bootstrap cell — invariant), a
  `samples = FALSE` consumer drops the column.

So `hc_draw_slice(scenario)` is `scenario_step_slice(scenario, "hc")` with the
readout fields replaced by the **design-wide aggregates**: `est_method` and
`proportion` become the union, `ci` and `samples` the `any`. The per-shard runner
`ssd_run_hc_step()` is unchanged — it already consumes vector `est_method`/
`proportion` and scalar `ci`/`samples` off its slice; the factory just hands it
the aggregated slice.

Consequence (accepted): because the readout union is a **design-wide** quantity,
the `hc` shard's bytes depend on it, so widening the union (adding a scenario that
introduces a new `proportion`/`est_method`, or the first `ci = TRUE`/
`samples = TRUE`) re-sigs and recomputes the `hc` shards. The `sample`/`fit`
shards are untouched (they do not depend on hc readouts). This is the bundle-and-
aggregate trade: simple, no hc-step refactor, at the cost of hc growth-robustness.

*Alternative considered:* make `proportion` a primer/bootstrap axis — rejected;
it gives each proportion an independent bootstrap (wasteful, and breaks the CRN
pairing of HC5 vs HC10 from one draw set). *Alternative considered:* split the hc
step into a shared draws shard + per-readout axis layer (growth-robust) —
rejected for this change (Non-Goal); revisitable if hc readout growth bites.

### Decision: `ssd_design(...)` owns naming, mirroring `ssd_data()`

A collection constructor returns a classed `ssdsims_design` named list of
scenarios. Names come from explicit argument names or are derived from the
argument expression (the `ssd_data()` derivation), validated unique, non-empty,
non-`NA`, and safe (`^[A-Za-z][A-Za-z0-9_]*$`). The name is used **only** as the
`scenario` identity-column value and the `summary_<name>` target-name suffix —
never in a shard path, target name, primer, or result value. Each element must be
an `ssdsims_scenario`. `ssd_design_targets()` accepts **only** this collection.

*Alternative considered:* a bare named list — rejected (two entry points to the
same validation).

### Decision: per-scenario summaries filter the shared shards; one combined summary

Each scenario gets a `summary_<name>` target that reads the shared `hc` shards
its tasks reference and filters to that scenario's `(proportion, est_method, ci,
samples)` readout slice and its cell slice, writing a per-scenario compact
summary. The single top-level `summary` target (the one name carrying no sig)
names every `summary_<name>` target, then unions their compact summaries into
`<root>/summary.parquet` via `ssd_summarise_design(summaries, path)` — each read
lazily through `duckplyr`, tagged with a literal `scenario` column, `union_all`-ed
inside DuckDB (never collected into R), survivors skipped if a member did not land
(the `ssd_summarise()` keep-going property). Retained-draws files stay
per-scenario.

### Decision: results navigation by recomputing sigs (no manifest)

A scenario object deterministically yields its per-step sigs and cells, so
read-back (`ssd_open_uploaded()`, the per-scenario summary) recomputes the paths
it needs; the combined `summary.parquet` (with its `scenario` column) is the
human entry point to results. No on-disk `scenario → sig` manifest is written.

*Alternative considered:* emit a `scenario → sig/paths` manifest for `ls`-style
discoverability — deferred; recomputation is lossless and a manifest is one more
artifact to keep in sync. Revisitable if operability needs it.

### Decision: upload mirrors the sig tree

With a non-`NULL` `upload`, each shard's paired `upload_<step>` target ships under
the *same* sig-addressed path (no `scenario=<name>` extension), so a shared shard
uploads once and the remote layout mirrors the local one; `ssd_open_uploaded()`
resolves a scenario's sigs to read it back. The factory performs no network I/O
(`ssd_upload_dryrun()` passes through).

### Decision: shared seeds are common random numbers, by design

Two scenarios with the same `seed` share `(seed, primer)` on overlapping task
identities, so their shared shards are *identical* — that identity is exactly
what licenses dedup, and it is the classic variance-reduction pairing for setting
comparisons. The factory neither warns nor validates seed distinctness; distinct
seeds give independent streams (and share nothing).

## Risks / Trade-offs

- **An incomplete `sig` would silently mis-share** (two scenarios with different
  bytes collapse to one shard) → the `sig` hashes `seed` + the *complete*
  `scenario_step_slice()` read-set (the package's source of truth for "what a
  step reads"); a sig-completeness test forks the sig on every off-axis setting
  that changes bytes (`nrow_max`, `dists`, and the hc readouts via the union).
  `targets`' own duplicate-name abort is a loud backstop, never a silent merge.
- **hc readout growth recomputes hc shards** (widening the union or flipping the
  first `ci`/`samples` to `TRUE`) → accepted trade of bundle-and-aggregate;
  `sample`/`fit` stay cached, and the split-step alternative is deferred, not
  precluded.
- **Two scenarios must emit byte-identical target definitions for a shared
  shard** (else the union dedup is unsound) → guaranteed by closing the command
  over the `scenario_step_slice()` projection (a pure function of materialised
  fields, no environment capture), the same contract that already makes per-step
  caches independent.
- **Constructed target names could collide** (a sig/cell suffix echoing another)
  → sig is a fixed-width hash and the cell suffix is structured; `targets` aborts
  on duplicate names at sourcing time.
- **`union_all` schema drift across scenarios** → the combined summary unions
  *this pipeline's* fresh per-scenario summaries (same package version), not
  historical files; cross-version aggregation is the deferred study level.

## Migration Plan

Additive only: two new exports, no signature or behaviour change to existing
functions, no re-baselining (per-task results are unchanged). Promotion of a flat
`ssd_scenario_targets()` run into a design recomputes (addressing differs), as
documented. Nothing to roll back beyond reverting the commit.

## Open Questions

- Whether to ship a design `inst/targets-templates/` template or fold a design
  section into the existing template and vignette — leaning vignette section +
  extended template comments (resolved during implementation).
- Whether results operability later warrants the deferred `scenario → sig`
  manifest — revisit if shard-level navigation without the scenario object is
  needed.
