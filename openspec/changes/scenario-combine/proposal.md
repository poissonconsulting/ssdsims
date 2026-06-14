# Proposal: scenario-combine

## Why

The point of running several scenarios as one pipeline is the **irregular
(ragged) design**: exploring **finer detail over a subset of the axes without
exploding the full cross-product**. A single `ssd_scenario` is one **regular**
grid — a rectangular cross-join. But a study usually wants a coarse grid
everywhere *and* a dense refinement in one region (more `nrow` values, only for
one dataset and one distribution set, say). Expressing that as one regular
scenario forces the dense axis onto every cell — computing a cross-product you
never wanted. Expressing it as N separate `_targets.R` stores recomputes the
overlap (the coarse cells the refinement shares) and gives no shared scheduling
and no single summary. The ROADMAP books this as ❗️ [scenario-combine]: *provide
a convenient way to run multiple `ssd_scenario` objects as a single targets
pipeline.*

This change introduces the **design** as that unit: a named set of scenarios
**unioned into one ragged task set** and run as one pipeline. It sits in the
`GLOSSARY.md` hierarchy **scenario ⊂ design ⊂ study** — a scenario is one regular
grid, a design unions scenarios into the full (possibly non-regular) experimental
region, and a study is a future read-side aggregate. Because the members of a
ragged design share most of their cells, the design **computes each shared cell
once**: the refinement reuses the coarse run's shards rather than recomputing
them. Comparing simulation settings across members is a **secondary** use the
same machinery also serves.

## What Changes

- **New `ssd_design(...)` collection constructor** — a validated, named
  collection of `ssdsims_scenario` objects (class `ssdsims_design`), mirroring the
  `ssd_data()` naming convention. Each name is a **scenario name** within the
  design, used **only** as the `scenario` identity-column value and the
  per-scenario summary target-name suffix — never in a shard path, shard target
  name, the per-task primer, or any result value. A design of **one** is valid and
  uniformly shaped.
- **A design is the ragged union of its members' task sets.** The factory unions
  every member's regular per-step task tables and **de-duplicates by task
  identity (cell)**: a cell several members share is one target, computed once;
  cells only one member reaches are computed once for it. This is the irregular
  grid — finer detail in a subregion without the full cross-product, and without
  recomputing the shared coarse cells.
- **Naked cell addressing — no per-scenario prefix, no opaque content hash.**
  Shards are written under the existing per-layout root
  `<root>/layout=<hash(partition_by)>` with a **legible `seed=<value>`** level
  (`<root>/seed=<value>/layout=<hash>/<step>/<cells>`), and target names are the
  plain `<step>_<cells>` (no `<name>_` prefix). Members may **vary the `seed`**;
  the `seed=` level keeps different-seed members from colliding while sharing
  everything within a seed. There is **no `sig=`**: with the consistency contract
  below, the same cell at the same seed and layout *is* the same bytes.
- **A name→value consistency contract.** `ssd_design()` validates that across
  members the same name means the same thing — same `dataset` name ⟹ identical
  data, same `min_pmix` name ⟹ identical function, same `distset` name ⟹ identical
  members — and that `partition_by` is uniform (so cells coincide). This is what
  makes naked cell addressing safe in place of a content hash.
- **`dists` is resolved design-wide as the union fit + `distset` subset.** Each
  scenario already fits the union `scenario$fit$dists` once and `hc` subsets it by
  the `distset` axis; members requesting different distribution sets share the
  union fit and differ only by `distset` cell — no `dists` fork.
- **`nrow_max` is a uniform draw-size guard; differing/changing it across members
  is documented as undefined behaviour** (no aggregation). Keep it uniform.
- **Per-overlap hc readout aggregation (not global).** The four non-axis hc
  settings are reduced **per shared hc cell, over only the members that touch that
  cell**: **`union()` over `proportion` and `est_method`**, **`any()` over `ci`
  and `samples`**. A cell only one member reaches keeps that member's (smaller)
  demand — so e.g. `ci = FALSE, nsim = 1000` beside `ci = TRUE, nsim = 10`
  bootstraps only the 10 overlapping sims, leaving 990 cheap. `nboot`/`ci_method`/
  `parametric`/`distset` stay **cell axes** (they are in the primer; aggregating
  them would move the RNG stream and break byte-identity). A `ci = FALSE` task's
  analytical `est` is served by a coincident `ci = TRUE` shard at the same
  `(fit-id, distset)` when one exists, else by its own `ci = FALSE` shard. This
  reuses `ssdtools::ssd_hc()`'s vectorized `proportion`/`est_method` and the
  package's `hc_collapse_est_methods()` — **no ssdtools refactor**.
- **New `ssd_design_targets(design, ..., root, upload, cue)` target factory** —
  emits the de-duplicated union of shard targets plus the combined summary; one
  `tar_make()` runs the whole design under one scheduler/controller.
- **Per-scenario summaries filtering shared shards, unioned into one table.** Each
  `summary_<name>` target reads the shared shards and filters to that scenario's
  cells and readout slice; the top-level `summary` target unions them into
  `<root>/summary.parquet` with a `scenario` identity column, at the DuckDB level.
- **Byte identity is preserved** — a member's per-task results are byte-identical
  to running it alone; combining changes addressing only, never `(seed, primer)`.
- `ssd_scenario_targets()`'s public contract is **unchanged** (it keeps
  `layout=` addressing without the `seed=` level); promoting a flat run into a
  design is documented as safe but recomputing.

## Capabilities

### New Capabilities
- `scenario-combine`: the **design** — `ssd_design()` with its name→value
  consistency contract; the ragged task-set union with naked cell addressing under
  `seed=`/`layout=`; `ssd_design_targets()`; the design-wide `dists` union; the
  per-overlap hc readout aggregation (`union` `proportion`/`est_method`, `any`
  `ci`/`samples`) with `ci`-routing; per-scenario summary filtering and the
  combined design summary with its `scenario` identity column.

### Modified Capabilities
<!-- None: `ssd_scenario_targets()`'s requirements, the `task-shards` /
     `shard-runner` per-task contracts, and `ssdtools` are untouched. The design
     factory unions and addresses the existing single-scenario machinery. -->

## Impact

- **New code**: `ssd_design()` (constructor + consistency validation,
  `R/design.R`); `ssd_design_targets()`, the ragged task-union + cell-dedup, the
  `seed=`-extended results-dir helper, the per-overlap hc demand aggregation +
  `ci`-routing, the per-scenario summary filter, and the combined-summary fan-in
  (`R/targets-runner.R` or a sibling file).
- **APIs**: two new exports (`ssd_design()`, `ssd_design_targets()`); no breaking
  change. **No ssdtools change.**
- **Docs**: README / `vignettes/sharded-pipeline.qmd` (a design section led by the
  irregular-grid use case, with setting-comparison secondary), `_pkgdown.yml`,
  `GLOSSARY.md` (*Design terms*), `ROADMAP.md`, regenerated `man/`.
- **Tests**: a ragged two-member design (coarse + refinement) sharing the
  overlapping cells (one target, byte-identical to standalone) and building the
  refinement's extra cells once; distinct-seed members landing under separate
  `seed=` trees sharing nothing; the consistency contract rejecting inconsistent
  name bindings; the per-overlap readout aggregation (the `ci`/`nsim` example);
  combined-summary content + `scenario` tag; upload shape.
- **Dependencies**: none (only GLOSSARY/vignette prose could brush against
  `scenario-input-types`).
