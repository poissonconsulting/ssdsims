## Why

`cost-analysis-targets` reads observed compute back from a single scenario's run
(`ssd_analyse_cost(scenario, ...)`); `scenario-combine` introduces the
**design** — several scenarios run as one `targets` pipeline, one store, one
combined `<root>/summary.parquet` carrying a `scenario` identity column. A study
run as a design therefore wants **one** observed-cost rollup across its members,
not N hand-stitched `ssd_analyse_cost(scenario)` calls: the design store already
holds every member's per-target `seconds`, and the combined summary already
carries every member's hc timing columns. This change closes that gap by
teaching the cost-analysis functions to accept an `ssdsims_design`. It is
**contingent on `ssd_design()`** and the design factory landing with
`scenario-combine`, and on the analyse/compare/recalibrate functions and the
`ssdsims_cost_analysis` object landing with `cost-analysis-targets`.

This change is structured in **two phases so work can start now**: a
collection-agnostic rollup **seam** (Phase A) that depends only on already-landed
code (`calibrate_coefficients()`/`calibrate_nrow_factor()`/
`new_ssdsims_cost_calibration()`/`cost_cpu_info()`/`format_duration()`/
`scenario_results_dir()` all exist today), and a thin `ssdsims_design`
**adapter** (Phase B) that lands when `scenario-combine` (and the scenario-level
`cost-analysis-targets` functions) arrive. The seam takes a *normalised*
representation — a named set of `(scenario, results-root)` members plus their
per-member scenario-level analyses — so it can be built and unit-tested now
against fixtures and real `ssdsims_scenario` objects, with no reference to the
unlanded `ssd_design()` / `ssd_analyse_cost()` symbols. Phase B is the adapter
that unpacks an `ssd_design()` into that representation and delegates to the seam.

## What Changes

### Phase A — the rollup seam (buildable now, depends only on landed code)

- **A collection-agnostic design rollup** over a named set of per-member
  scenario analyses: row-bind the members' `ci_method` × `nboot` breakdowns into
  one breakdown with a leading `scenario` column, and reduce to design totals
  (observed total = Σ member totals; observed longest = max member longest), with
  members that did not run skipped and the contributing-member count reported.
- **Pooled, host-aware recalibration over per-member measured sweep frames** —
  pools the members' measured `(nrow, ci_method, nboot, time, .host)` rows into
  one frame and calls the existing `calibrate_coefficients()`/
  `calibrate_nrow_factor()` to return an `ssdsims_cost_calibration` usable
  directly by `ssd_estimate_cost()` (both landed); distinct `.host` values are
  not silently pooled (explicit host or abort). This phase is **end-to-end real**
  today: scenario *name* never enters the cost model, so pooling needs no design
  object.
- **Per-member addressing derivation** — from a named set of scenarios and a
  root, derive each member's `scenario=<name>` results root (via the landed
  `scenario_results_dir()`) and `<name>_` target-name prefix — the addressing
  `scenario-combine` will mint, computed without `ssd_design()`.
- **A design-breakdown formatter** — a pure string helper (reusing
  `format_duration()`) that renders the per-`scenario` breakdown and design
  totals, ready for Phase B's print methods to call (no S3 method on the
  sibling-owned `ssdsims_cost_analysis` class is defined in this phase, to avoid
  a merge collision).

### Phase B — the `ssdsims_design` adapter (gated on the siblings landing)

- **`ssd_analyse_cost()` gains an `ssdsims_design` method.** Given a design and
  its run `root`, it reads each member's observed cost from its
  `<root>/scenario=<name>/layout=<hash>` tree and returns an
  `ssdsims_cost_analysis` whose breakdown carries a leading `scenario` column
  (member name) above the existing `ci_method` × `nboot` axes, plus design-level
  totals (observed total compute and observed longest single task across all
  members). It is read-only like the scenario method (no pipeline, no RNG, no
  writes).
- **The combined `<root>/summary.parquet` is the convenient hc read surface.**
  Because `scenario-combine` unions the per-scenario *compact* summaries — which
  `cost-analysis-targets` keeps the `.start`/`.end`/`.host` columns on — the
  combined summary already carries every member's hc timings tagged by
  `scenario`, so design hc cost is one DuckDB read with no per-scenario glob.
  The `fit` layer (the addend) still comes from the per-member shard globs, as at
  the scenario level.
- **The `targets` store path becomes design-aware.** A design is **one** store,
  so a single `tar_meta()` covers every member; target names carry the design's
  `<name>_` prefix (`<name>_<step>_step_<pathcell>`). The target-name resolver
  regenerates the expected names **per member, with the design prefix and the
  member's `scenario=<name>` root** (reusing `scenario-combine`'s prefix/root
  logic), and joins on the store's `name` column — never string-parsing. Per-shard
  envelope overhead and the pre-timing fallback work per member, summed to the
  design total; unmatched targets are reported, not dropped.
- **`ssd_compare_cost()` gains an `ssdsims_design` method.** It places the design
  predicted cost (the sum of the members' `ssd_estimate_cost()` predictions)
  beside the design observed analysis and reports design-total predicted/observed
  ratios plus a per-`scenario` ratio row, returning an `ssdsims_cost_comparison`.
- **`ssd_calibrate_cost_from_run()` gains an `ssdsims_design` method** that reads
  the members' measured hc/fit durations and delegates to the Phase A pooled
  host-aware recalibration seam (so the pooling/host logic is the same code the
  scenario path will reach).
- **Print/format methods become design-aware.** `ssdsims_cost_analysis` /
  `ssdsims_cost_comparison` print a per-scenario breakdown and the design totals
  (via the Phase A breakdown formatter) when built from a design; the
  scenario-level output is unchanged.
- **Cost-analysis vignette gains a design section** demonstrating the design-level
  analyse → compare → recalibrate rollup over a small two-scenario design.

## Capabilities

### New Capabilities
<!-- None: this extends the existing cost-analysis capability rather than adding one. -->

### Modified Capabilities
- `cost-analysis`: the analyse / compare / recalibrate functions, their
  target-name resolver, and the `ssdsims_cost_analysis` / `ssdsims_cost_comparison`
  objects gain `ssdsims_design` dispatch — design-level observed totals, a
  per-`scenario` breakdown dimension, the combined-summary hc read surface, the
  one-store design-aware `tar_meta()` resolver (prefix- and `scenario=<name>`-root
  aware), pooled host-aware recalibration across members, and design-aware
  printing. The scenario-level requirements are unchanged.

## Impact

- **Phased so Phase A lands now and Phase B completes on `scenario-combine`.**
  Phase A (the rollup seam) depends only on already-landed code and ships
  immediately as internal, unexported helpers with fixture/real-scenario tests —
  it adds no exports, no `man/`, no `_pkgdown.yml` entries, and defines no S3
  method on a sibling-owned class, so it cannot collide at merge. Phase B (the
  `ssdsims_design` adapter) requires `cost-analysis-targets` (the
  `ssd_analyse_cost()`/`ssd_compare_cost()`/`ssd_calibrate_cost_from_run()`
  functions, the `ssdsims_cost_analysis`/`ssdsims_cost_comparison` objects, the
  scenario tar_meta resolver, the timing columns on the compact summary) and
  `scenario-combine` (`ssd_design()`/`ssdsims_design`, `ssd_design_targets()`'s
  `<name>_` prefix, the `scenario=<name>/layout=<hash>` roots, the combined
  `<root>/summary.parquet`). The dependency **gate moves to Phase B only**: Phase
  A is the load-bearing logic, Phase B is the thin adapter that wires it to the
  real objects.
- **Emergent cross-change property relied on:** the combined summary carries hc
  timings only because `cost-analysis-targets` keeps them on the compact summary
  *and* `scenario-combine` unions the compact summaries verbatim. This change
  consumes that property; if either sibling drops it, the design hc read falls
  back to the per-member shard glob (no correctness loss, just no single-read
  shortcut).
- **New code (Phase A, now):** the rollup seam in a new `R/cost-analysis-design.R`
  — `combine_cost_breakdowns()`, `design_cost_totals()`,
  `pool_calibration_from_frames()`, `design_member_addressing()`,
  `format_design_breakdown()` — all internal/unexported, reusing the landed
  fitters/formatters. **New code (Phase B, gated):** the `ssdsims_design` branches
  of analyse/compare/recalibrate and the design-aware `format`/`print` methods,
  reusing `scenario-combine`'s prefix/root helpers and `cost-analysis-targets`'s
  scenario resolver, all delegating to the Phase A seam.
- **APIs:** no new exported functions in either phase — Phase A adds internals
  only; Phase B gives the three existing exports an `ssdsims_design` method. No
  breaking change to the scenario-level contract.
- **Tests:** Phase A (now) — `combine_cost_breakdowns()` builds the `scenario`
  column; `design_cost_totals()` sums/maxes and skips non-running members;
  `pool_calibration_from_frames()` pools single-host frames into a calibration
  usable by `ssd_estimate_cost()` and aborts on mixed hosts;
  `design_member_addressing()` derives `scenario=<name>` roots and `<name>_`
  prefixes from real scenarios; `format_design_breakdown()` snapshot. Phase B
  (gated) — end-to-end two-scenario design analysis, the prefixed-name store
  resolver, and the combined-summary hc read path.
- **Docs:** a design section in the cost-analysis vignette and `_pkgdown.yml`/
  `man/` regeneration land in **Phase B** (Phase A exports nothing to document);
  `ROADMAP.md` entry updated now.
- **Non-goals:** the **study** level (cross-design / cross-version / cross-host
  aggregation — `scenario-combine`'s deferred read-side concept); any change to
  the cost model's form, to timing capture, or to the scenario-level functions'
  behaviour; cross-scenario shard dedup.
