# Design: scenario-combine

## Context

`ssd_scenario_targets()` is a target factory: sourcing `_targets.R` computes the
shard tables and returns one named `format = "file"` target per `partition_by`
path cell per step, plus a `summary` fan-in, all under
`scenario_results_dir(scenario)` = `<root>/layout=<hash(partition_by)>`. The
per-task RNG is `(seed, primer)`, where `primer = task_primer()` over
`task_axes(step)` — the **task axes only**, never the scenario-wide settings. A
scenario is one **regular** cross-join of those axes.

The motivating need is the **irregular (ragged) design**: a coarse grid
everywhere plus a dense refinement in a subregion. That cannot be one regular
scenario (the dense axis would multiply every cell) nor N stores (recomputing the
shared coarse cells, no shared scheduler/summary). A design is the **union of
several regular grids** into the ragged region.

Two facts shape the factory:

- **Cells already dedup.** A step target's name and path derive from
  `task_axes`/`partition_by` cell values. Two members with the same cell mint the
  same name — so a *naked* (un-prefixed) union de-duplicates the shared cells
  automatically: shared work is done once, and the ragged union is exactly the set
  of cells some member wants (never the cross-product).
- **Same cell ⟹ same bytes only if the off-axis inputs agree.** A shard's bytes
  are `g(RNG(seed, primer), + the off-axis values the runner reads)`
  (`scenario_step_slice()` is the authoritative read-set). Within one scenario
  those are fixed; across members they must be made to agree, or two members could
  write different bytes to one cell. The earlier draft guarded this with an opaque
  per-step content hash (`sig=`); this revision instead **pins the off-axis inputs
  by contract and addresses the one genuinely variable one (`seed`) legibly.**

## Goals / Non-Goals

**Goals:**

- One `_targets.R`, one store, one `tar_make()` for a whole design.
- **Ragged union with shared cells:** the design computes exactly the union of its
  members' cells, each shared cell once — finer detail in a subregion without the
  full cross-product and without recomputing the coarse overlap.
- Per-member results byte-identical to running each member alone (combining
  changes *addressing* only, never `(seed, primer)`).
- Support members that **vary the `seed`** (e.g. repeating the exploration under
  several master seeds) in one pipeline.
- A combined design summary (compact estimate table) with a `scenario` identity
  column, each member's rows filtered from the shared shards and unioned at the
  DuckDB level.
- **No ssdtools refactor** — the hc readout aggregation rides
  `ssdtools::ssd_hc()`'s vectorized `proportion`/`est_method` and
  `hc_collapse_est_methods()`.
- `ssd_scenario_targets()`'s public contract unchanged.

**Non-Goals:**

- **The study level** — a read-side union across design-runs; deferred.
- **An opaque per-step content key (`sig=`).** Replaced by the name→value
  consistency contract plus the `seed=` level (see decisions).
- **`nrow_max` aggregation.** Treated as a uniform guard; differing/changing it
  across members is documented undefined behaviour.
- **Splitting the hc step into a draws shard + readout layer**, or aggregating the
  draw-shaping axes (`nboot`/`ci_method`/`parametric`). They stay cell axes.
- **A `name` field on the scenario object** — names live on the design collection.
- The legacy `ssd_run_scenario()` path, the manifest, and replay tooling.

## Decisions

### Decision: a design is the de-duplicated union of its members' task sets

`ssd_design_targets()` builds each member's regular per-step task tables, unions
them, and de-duplicates by task identity (`<step>_id`, the cell key). The returned
target list is the union of every member's shard targets keyed by the plain
`<step>_<cells>` name, so a cell several members share resolves to **one** target,
`tar_make()` computes it **once**, and the union is exactly the ragged set of
cells. A member's command closes over the per-step slice (not the whole scenario),
so two members' shared cells produce byte-identical target definitions.

*Alternative considered:* per-scenario prefixed target sets concatenated (the
first draft) — rejected; it forbids sharing and races two targets onto one path.
*Alternative considered:* merge members into one regular scenario — rejected; it
re-rectangularises the ragged region and over-computes the cross-product.

### Decision: naked cell addressing under `seed=` / `layout=`, no `sig=`

Shards are written at `<root>/seed=<value>/layout=<hash(partition_by)>/<step>/<cells>/part.parquet`
and named `<step>_<cells>`. The off-axis inputs to a shard's bytes are pinned so
that *cell + seed + layout ⟹ identical bytes*:

- **`seed`** is the one off-axis input that legitimately varies across members, so
  it becomes a legible top directory level. Members sharing a `seed` share every
  cell within it (common random numbers); members with distinct seeds share
  nothing and live under separate `seed=` trees.
- **`partition_by`** is already the `layout=` hash and must be **uniform** across
  members for cells to coincide.
- Every other off-axis input is pinned by the consistency contract or resolved by
  aggregation (below).

This drops the opaque `sig` entirely: a hash of `scenario_step_slice()` would
re-encode exactly what the contract now guarantees, at the cost of an
unreadable path and false splits when two members express the same binding
differently.

*Alternative considered:* keep `sig=` to make off-axis heterogeneity *safe but
wasteful* — rejected; the contract makes heterogeneity *loud* (rejected at
construction) and the tree legible. *Alternative considered:* pin `seed` too and
drop `seed=` — rejected; varying the master seed in one pipeline is a wanted use.

### Decision: a name→value consistency contract on `ssd_design()`

Because the package addresses `dataset`, `min_pmix`, and `distset` **by name**
and carries the values off-axis, two members could agree on a cell yet disagree on
what a name means — a silent same-cell/different-bytes corruption. `ssd_design()`
therefore validates across members: same `dataset` name ⟹ identical data; same
`min_pmix` name ⟹ identical function; same `distset` name ⟹ identical member set;
and `partition_by` identical. Inconsistent bindings abort with an informative
error naming the offending name. This is the safety that replaces `sig`.

### Decision: `dists` is resolved design-wide (union fit + `distset` subset)

Confirmed in code: `fit` fits the union `scenario$fit$dists` once per fit cell
(no `dists` axis in the fit grid), and `hc` carries `distset` as an axis,
subsetting the union fit to the named set's members and re-averaging. So members
requesting different distribution sets share the union fit and differ only by the
`distset` cell — `dists` never forks a fit shard. (Members must name shared
distsets consistently, per the contract.)

### Decision: per-overlap hc readout aggregation (`union` / `any`), not global

The four **non-axis** hc settings are aggregated **per shared hc cell, over only
the members whose task set contains that cell**: `proportion` and `est_method`
are **unioned**, `ci` and `samples` reduced with **`any()`**. A cell only one
member reaches keeps that member's smaller demand, so the expensive bootstrap is
done only where a `ci = TRUE` member actually has tasks — e.g. `ci = FALSE,
nsim = 1000` beside `ci = TRUE, nsim = 10` bootstraps the 10 overlapping sims and
leaves the other 990 as cheap `ci = FALSE`. Mechanically: union the members' hc
tasks (each tagged with its source scenario and demand), group by the hc cell
axes, reduce the demand columns.

The draw-shaping axes `nboot`/`ci_method`/`parametric` (and `distset`) **stay cell
axes** — they are in the primer, so folding them into the demand would move the
RNG stream and break byte-identity. The aggregated set is therefore exactly the
four settings that are *not* axes.

**`ci`'s NA-collapse** is the one wrinkle: a `ci = FALSE` task collapses
`nboot`/`ci_method`/`parametric` to `NA`, so its cell differs from a `ci = TRUE`
task at the same sim. The point `est` is analytical and bootstrap-config-invariant
(`hc_collapse_est_methods()`'s `analytical_est`), so a `ci = FALSE` task's `est`
is served by a coincident `ci = TRUE` shard at the same `(fit-id, distset)` when
one exists, and only mints its own `ci = FALSE` shard where no `ci = TRUE` member
overlaps. The set of computed hc shards is thus: every `ci = TRUE` member's cells,
plus `ci = FALSE` cells with no overlapping `ci = TRUE` shard — each carrying the
unioned `est_method`/`proportion` and `any` `samples` over the members it serves.

Byte-identity holds: a served `ci = FALSE` `est` equals the standalone `ci = FALSE`
value (same analytical computation), and a `ci = TRUE` member's CI uses its own
cell's `(nboot, ci_method, parametric)` primer. The per-shard `ssd_run_hc_step()`
is unchanged — it already consumes vector `est_method`/`proportion` and scalar
`ci`/`samples` off its slice; the factory hands it the per-cell aggregated demand.

*Alternative considered:* global `any(ci)` / `union` across the whole design —
rejected; it bootstraps cells no `ci = TRUE` member reaches (the 990 wasted sims).

### Decision: per-scenario summaries filter the shared shards; one combined summary

Each member gets a `summary_<name>` target that reads the shared shards its tasks
reference and filters to that member's cells and `(proportion, est_method, ci,
samples)` readout slice, writing a per-scenario compact summary. The single
top-level `summary` target names every `summary_<name>` target, then unions their
compact summaries into `<root>/summary.parquet` via
`ssd_summarise_design(summaries, path)` — each read lazily through `duckplyr`,
tagged with a literal `scenario` column, `union_all`-ed inside DuckDB (never
collected into R), survivors skipped if a member did not land. Retained-draws
files stay per-scenario.

### Decision: upload mirrors the addressed tree

With a non-`NULL` `upload`, each shard's `upload_<step>` target ships under the
same `seed=`/`layout=`/`<cells>` path (no per-scenario prefix), so a shared shard
uploads once and the remote mirrors the local tree; read-back resolves the cells.
The factory performs no network I/O.

## Risks / Trade-offs

- **A violated consistency contract would silently mis-share** → the contract is
  validated at construction and aborts loudly; `targets`' duplicate-name check is
  a second backstop. The contract is the explicit replacement for `sig`'s implicit
  guard.
- **Differing/changing `nrow_max` across members** → documented undefined
  behaviour; keep `nrow_max` uniform. (For `replace = FALSE` with the high default
  it is already irrelevant; the hazard is a member that lowers it.)
- **`ci`-routing complexity** (mapping a `ci = FALSE` task to a serving
  `ci = TRUE` shard) → contained to the factory's task-union bookkeeping; the
  runner and `ssd_hc()` are untouched, and byte-identity is preserved by the
  analytical-`est` invariance the package already relies on.
- **`union_all` schema drift across members** → the combined summary unions *this
  pipeline's* fresh per-member summaries (one package version), not historical
  files; cross-version aggregation is the deferred study level.

## Migration Plan

Additive only: two new exports, no signature or behaviour change to existing
functions, no re-baselining (per-task results unchanged). Promotion of a flat
`ssd_scenario_targets()` run into a design recomputes (it gains the `seed=` level),
as documented. Nothing to roll back beyond reverting the commit.

## Open Questions

- Whether to ship a design `inst/targets-templates/` template or fold a design
  section into the existing template and vignette — leaning vignette section +
  extended template comments (resolved during implementation).
- Whether to later expose between-seed variance as a first-class read-side
  grouping over the `seed=` trees — deferred to the study level.
