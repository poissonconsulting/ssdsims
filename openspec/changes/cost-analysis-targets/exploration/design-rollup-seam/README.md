# Proof of work: design-level cost-analysis rollup seam

**Status: prototype, not ready for prime time.** This is exploratory code, kept
with the change as proof of work. It is **not** part of the package build (the
root `.Rbuildignore` excludes `^openspec$`), so nothing here is loaded, exported,
or tested by the package suite.

## What this is

A working prototype of the *collection-agnostic rollup seam* for design-level
cost analysis — the load-bearing aggregation that the eventual `ssdsims_design`
methods (the scenario-combine-dependent tasks folded into this change's
`tasks.md`) will delegate to. It was written and unit-tested against
already-landed code (`calibrate_coefficients()`/`calibrate_nrow_factor()`/
`new_ssdsims_cost_calibration()`/`cost_cpu_info()`/`format_duration()`/
`scenario_results_dir()`) to prove the seam is buildable independently of
`ssd_design()`.

- `cost-analysis-design.R` — five internal helpers:
  - `combine_cost_breakdowns()` — row-bind per-member breakdowns, tag with a
    leading `scenario` column, drop non-running members.
  - `design_cost_totals()` — design total = Σ member totals, longest = max member
    longest, with the contributing-member count.
  - `pool_calibration_from_frames()` — host-aware pooled recalibration across
    members (mixed `.host` aborts unless one is selected).
  - `design_member_addressing()` — derive each member's `scenario=<name>` root and
    `<name>_` prefix without an `ssd_design()` object.
  - `format_design_breakdown()` — pure design-aware breakdown renderer.
- `test-cost-analysis-design.R` + `_snaps/` — the prototype's tests and snapshot
  (passed 28/28 when this code lived under `R/`/`tests/`).

## Why it lives here

The design-level work was originally drafted as a standalone `cost-analysis-design`
change. It is now folded into `cost-analysis-targets`: only the
**scenario-combine-dependent** tasks remain in `tasks.md` (the thin `ssdsims_design`
adapter), and this prototype seam is demoted to proof of work rather than shipped
ahead of its dependencies. When `scenario-combine` and the scenario-level
cost-analysis functions land, promote this logic into `R/` (likely
`R/cost-analysis.R`) behind the `ssdsims_design` methods and restore the tests to
`tests/testthat/`.
