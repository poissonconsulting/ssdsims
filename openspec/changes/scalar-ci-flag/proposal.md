## Why

`ssd_define_scenario()` currently treats `ci` as a **grid/task axis**: it accepts a length-2 `c(FALSE, TRUE)` vector (`chk_length(ci, upper = 2L)`), `task_axes("hc")` lists `"ci"`, and a dedicated *"ci = FALSE collapse"* (`TARGETS-DESIGN.md` §1.2 + the `task-lists` *"honouring the ci = FALSE collapse"* requirement) keeps the `ci = FALSE` portion of a mixed grid from fanning out across the bootstrap-only knobs. The `scenario-definition` spec even instructs users to *"set `ci = c(FALSE, TRUE)` … to reduce the scenario's asymmetry."*

That guidance rests on a premise that does not hold. Verified against `ssdtools` 2.6.0.9002: the point estimate `est` returned by `ssd_hc()` is **byte-identical** whether `ci = TRUE` or `ci = FALSE`, across every `ci_method` (`arithmetic_samples`, `geometric_samples`, `GMACL`, `MACL`, `multi_fixed`, `multi_free`, `weighted_samples`) and independent of the RNG state — the estimate is computed analytically from the fit and never touches the bootstrap. A `ci = TRUE` run is therefore a **strict superset** of a `ci = FALSE` run: same `est`, same `wt`/`dist`/`proportion`/`pboot`, plus the populated `se`/`lcl`/`ucl`. The only difference a `ci = FALSE` row carries is `nboot = 0` (cosmetic) and `NA` CI columns. Running `ci = c(FALSE, TRUE)` in one scenario thus only **doubles the hc work to emit a redundant point-estimate row per fit-task** — it adds no information.

This is precisely the *"a single `TRUE` is a superset of `FALSE`"* property the design already invokes to keep `samples` a **scalar, non-axis** knob (the `samples` requirement, same spec). `ci` should be treated the same way. Choosing `ci` is a scenario-wide **either/or**: `ci = FALSE` to skip the (expensive) bootstrap and get point estimates only, or `ci = TRUE` to get estimates *plus* CIs — never both.

## What Changes

- **`ci` becomes a scalar flag** in `ssd_define_scenario()` (`chk::chk_flag`, default `FALSE`), no longer a length-2 axis. Passing `ci = c(FALSE, TRUE)` (or any non-flag) SHALL abort in the user-facing frame, mirroring `samples`.
- **`ci` leaves the `hc` axis vocabulary**: `task_axes("hc")` drops `"ci"`, so `ci` is neither a path nor an inner axis and does **not** enter the per-task RNG primer. It is applied uniformly to every hc task (like `samples`), carried via the scenario's `hc` slice rather than crossed.
- **The `ci = FALSE` collapse machinery is retired.** `hc_grid_tbl()` (`R/task-lists.R`) loses the `any(ci == FALSE)` / `any(ci == TRUE)` `bind_rows` branching and becomes a single grid: `ci = FALSE` yields one row per `est_method` with the bootstrap-only knobs (`nboot`/`ci_method`/`parametric`) canonically `NA` (so they cannot leak into task identity); `ci = TRUE` fans out over `nboot × est_method × ci_method × parametric` as today.
- **The bootstrap-knob guard stays, simplified.** When the scalar `ci = FALSE`, supplying `nboot`/`ci_method`/`parametric` is still an error — but the escape hatch becomes `ci = TRUE` (not `ci = c(FALSE, TRUE)`).
- **`ssd_hc_sims()` validates `ci` as a flag** so the low-level entry point rejects a vector `ci` too (it already applies `ci` as a scalar — it is not in that function's factorial expansion).

## Capabilities

### Modified Capabilities
- `scenario-definition`: `ci` is demoted from a grid/task axis to a scalar flag (default `FALSE`), removed from the `hc` `task_axes()` vocabulary and the per-task primer, and applied uniformly. The standalone *"ci = FALSE rejects bootstrap-only knobs"* requirement is folded into the new scalar-`ci` requirement (the rejection survives; the `c(FALSE, TRUE)` enablement story is replaced by `ci = TRUE`).
- `task-lists`: the hc task-table derivation no longer performs the §1.2 *ci = FALSE collapse* (there is no `ci` axis to collapse); it crosses each fit-task identity with the hc grid and applies the scalar `ci`, with bootstrap-only knobs `NA` when `ci = FALSE`.
- `hazard-concentrations`: `ssd_hc_sims()` validates `ci` as a flag (it was already scalar in the factorial expansion).

## Impact

- **Code**: `R/scenario.R` (`chk_flag(ci)`; simplify the bootstrap-knob guard + message), `R/task-lists.R` (`task_axes("hc")` drops `"ci"`; `hc_grid_tbl()` becomes a single-branch grid), `R/hc-sims.R` (`chk_flag(ci)`). Docs/`man/` for `ssd_define_scenario()` (the *"# `ci = FALSE`"* roxygen section) and `ssd_hc_sims()`.
- **Behaviour (breaking, pre-release)**: `ci = c(FALSE, TRUE)` now errors; callers pick a scalar. Estimates are unchanged. Because `ci` leaves the primer, the per-task RNG stream for the hc step shifts, so **bootstrap CIs (`lcl`/`ucl`/`se`) change value** for a given seed (point estimates do not); acceptable for an unreleased package with no downstream dependants (`TARGETS-DESIGN.md` §12). Snapshot tests (`hc_sims1`, `hc_sims1ci`, scenario/task-table snaps) re-baseline.
- **Docs**: `TARGETS-DESIGN.md` §1.2 (the *ci = FALSE collapse*) is retired and §12 gains a `scalar-ci-flag` Cleanup bullet (see below).
- **Dependencies (DAG)**: an **independent tidy-up** with no prerequisites and no dependants — **not** on the `TARGETS-DESIGN.md` §12 dependency DAG (§12 "Cleanup"), alongside `cleanup-as-ssd-data` / `blob-storage-format` / `error-call-origin`. Can land at any time.
- **Cross-reference — `migrate-public-api`**: that in-flight change's *"Reproducible bootstrapping"* requirement lists `ci` among the hc primer-identity fields (`ci, nboot, est_method, ci_method, parametric`). Since `ci` becomes a scenario-wide constant, it no longer belongs in the per-task identity; whichever change lands second SHALL drop `"ci"` from that enumeration. The two are otherwise orthogonal and may land in either order.
