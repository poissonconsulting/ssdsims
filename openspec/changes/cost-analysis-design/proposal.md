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

## What Changes

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
- **`ssd_calibrate_cost_from_run()` gains an `ssdsims_design` method.** It pools
  the members' **measured** hc task durations into one sweep frame and re-fits the
  same per-task model — scenario *name* never enters the cost model, so pooling
  across members is sound, but it stays **host-aware**: mixed `.host` values
  across the design are not silently pooled (explicit host selection or abort
  listing them), exactly as the scenario method.
- **Print/format methods become design-aware.** `ssdsims_cost_analysis` /
  `ssdsims_cost_comparison` print a per-scenario breakdown and the design totals
  when built from a design; the scenario-level output is unchanged.
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

- **Stacks on two in-flight changes — lands after both.** Requires
  `cost-analysis-targets` (the `ssd_analyse_cost()`/`ssd_compare_cost()`/
  `ssd_calibrate_cost_from_run()` functions, the `ssdsims_cost_analysis`/
  `ssdsims_cost_comparison` S3 objects, the tar_meta resolver, the timing columns
  retained on the compact summary) and `scenario-combine` (`ssd_design()` /
  `ssdsims_design`, `ssd_design_targets()`'s `<name>_` target-name prefix, the
  `scenario=<name>/layout=<hash>` roots, and the combined `<root>/summary.parquet`
  with its `scenario` column). Neither dependency function exists in the codebase
  today, so this change's tasks cannot be implemented until both land.
- **Emergent cross-change property relied on:** the combined summary carries hc
  timings only because `cost-analysis-targets` keeps them on the compact summary
  *and* `scenario-combine` unions the compact summaries verbatim. This change
  consumes that property; if either sibling drops it, the design hc read falls
  back to the per-member shard glob (no correctness loss, just no single-read
  shortcut).
- **New code:** design methods/dispatch in `R/cost-analysis.R` (the
  `ssdsims_design` branches of analyse/compare/recalibrate, the `scenario` column
  in the breakdown, design-aware `format`/`print`), reusing `scenario-combine`'s
  internal prefix/root helpers and `cost-analysis-targets`'s resolver and fitters.
- **APIs:** no new exported functions — the three existing exports gain an
  `ssdsims_design` method; no breaking change to the scenario-level contract.
- **Tests:** a two-scenario design analysis (design totals = sum of per-member
  analyses; per-`scenario` breakdown), the store resolver matching prefixed target
  names across the `scenario=<name>` roots, pooled host-aware recalibration
  (mixed-host design aborts), and the combined-summary hc read path.
- **Docs:** a design section in the cost-analysis vignette; `_pkgdown.yml` and
  `man/` regenerate for the documented design methods; `ROADMAP.md` entry.
- **Non-goals:** the **study** level (cross-design / cross-version / cross-host
  aggregation — `scenario-combine`'s deferred read-side concept); any change to
  the cost model's form, to timing capture, or to the scenario-level functions'
  behaviour; cross-scenario shard dedup.
