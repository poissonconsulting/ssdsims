## 1. Constructor and validation (`R/scenario.R`)

- [x] 1.1 Replace the `ci` validation (`chk_logical` + `chk_not_any_na` + `chk_unique` + `chk_length(ci, upper = 2L)`) with `chk::chk_flag(ci)`; keep the default `ci = FALSE`
- [x] 1.2 Simplify the bootstrap-option guard from `if (length(ci) == 1L && isFALSE(ci))` to `if (isFALSE(ci))`, and change the message escape hatch from "Set `ci = c(FALSE, TRUE)` to enable bootstrap" to "Set `ci = TRUE` to enable bootstrap"
- [x] 1.3 Update the `ssd_define_scenario()` roxygen: the *"# `ci = FALSE`"* section (scalar flag; estimate invariant; `ci = TRUE` superset; `ci = FALSE` cheap/bootstrap-free) **and** the `@param partition_by` line that enumerates the hc vocabulary (`hc` adds `ci`, `nboot`, …) — drop `ci` there
- [x] 1.4 Reorder the `ssd_define_scenario()` signature by role so the **simulation settings** are contiguous: keep data/`seed`/`nsim`/`name` first, then the cross-join axes (`nrow`, `replace`, `dists`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`, `nboot`, `est_method`, `ci_method`, `parametric`), then the simulation settings (`proportion`, `ci`, `samples`), then `partition_by`, `bundle`, `upload`. Concretely, move `proportion = 0.05` and `ci = FALSE` down from inside the hc axes to sit before `samples = FALSE`
- [x] 1.5 Mirror the role order in the stored `hc` list (`scenario$hc <- list(nboot, est_method, ci_method, parametric, proportion, ci, samples)`) and reorder the roxygen `@param`/`@inheritParams` so the generated `man/` lists `proportion`/`ci`/`samples` together
- [x] 1.6 Confirm `print.ssdsims_scenario()` renders the hc scenario options in the new role order — hc axes first, then the simulation settings `proportion`/`ci`/`samples` (covered by the print snapshot in §9)

## 2. Task axes and hc grid (`R/task-lists.R`)

- [x] 2.1 Remove `"ci"` from `task_axes("hc")` (hc vocabulary becomes the fit axes plus `nboot`, `est_method`, `ci_method`, `parametric`)
- [x] 2.2 Rewrite `hc_grid_tbl()` as a single-branch grid keyed by the scalar `scenario$hc$ci`: `ci = FALSE` → one row per `est_method` with `nboot = NA_integer_`, `ci_method = NA_character_`, `parametric = NA`; `ci = TRUE` → `expand_grid` over `nboot × est_method × ci_method × parametric`. Drop the `any(ci == FALSE)` / `any(ci == TRUE)` `bind_rows` branching. Keep emitting `ci` as a (now single-valued) **carried column** so the runners can read it
- [x] 2.3 Update the `ssd_scenario_hc_tasks()` roxygen (the collapse description and the `ci = c(FALSE, TRUE)` example) to the scalar-`ci` ground truth
- [x] 2.4 Verify the in-memory baseline runner path (`hc_args <- hc_tbl[c("ci", "nboot", …)]` → `pmap(hc_data_task_primer, …)`) still reads `ci` from the carried column — **no change expected** (this is the `n_max` carried-column pattern); add a comment if it aids clarity

## 3. Step runner shared by the single-core and targets pipelines (`R/targets-runner.R`, `R/shard-runner.R`)

- [x] 3.1 `ssd_run_hc_step()` reads `ci = t$ci` from the carried column — confirm this still holds with `ci` out of `task_axes("hc")` (**no code change expected**, since the column persists); this one function backs both `ssd_run_scenario_sharded()` (single-core) and the `tar_map` hc target
- [x] 3.2 Update `ssd_run_hc_step()` roxygen (the *"`ci = FALSE` collapse, section 1.2"* mention) to "scalar `ci`; bootstrap-only scenario options `NA` when `ci = FALSE`"
- [x] 3.3 Confirm the shard write/read round-trips `ci` as a non-axis carried column exactly as it already does for `n_max` (no path-key/partition involvement); add a targeted check in §9 rather than new runner code

## 4. Per-task primer identity (`R/task-primer.R`)

- [x] 4.1 Update the `task_primer()` roxygen (the hc bullet that lists `ci` among the hc-grid identity fields) to drop `ci` — it is a carried column, not part of task identity

## 5. Partitioning / sharding vocabulary (`R/scenario.R`, `R/task-shards.R`)

- [x] 5.1 Because `task_axes("hc")` no longer contains `"ci"`, `partition_by$hc = c(..., "ci")` (and `bundle$hc` naming `"ci"`) now correctly hit the "unknown axis" path in `validate_axis_list()` — **no code change**, but add the regression test in §9
- [x] 5.2 Update the `ssd_scenario_hc_shards()` roxygen example in `R/task-shards.R` (`ci = c(FALSE, TRUE)` → scalar `ci = TRUE`)

## 6. Low-level entry point and legacy runner (`R/hc-sims.R`, `R/run-scenario.R`, `R/internal.R`)

- [x] 6.1 Add `chk::chk_flag(ci)` to `ssd_hc_sims()` so a vector `ci` is rejected at that layer (it already applies `ci` as a scalar — `ci` is not in its `expand_grid`)
- [x] 6.2 Confirm the legacy `ssd_run_scenario.*` methods and the `internal.R` `do.call(ssd_hc_sims, …)` helper pass `ci` straight through — a vector `ci` now surfaces the §6.1 flag error from `ssd_hc_sims()` (no axis logic of their own; no change beyond inheriting the validation)

## 7. Params, generated docs, and design docs

- [x] 7.1 `R/params.R`: tighten the `@param ci` text to "scalar flag … (not a cross-join axis; the point estimate is identical whether `TRUE`/`FALSE`)"
- [x] 7.2 Run `devtools::document()` so `man/ssd_define_scenario.Rd`, `man/ssd_hc_sims.Rd`, `man/ssd_scenario_hc_shards.Rd`, and the partition vocabulary docs regenerate
- [x] 7.3 (Done in this change) `TARGETS-DESIGN.md` §1.2 rewritten to "`ci` is a scalar flag (not an axis)"; §1/§5 sidebar, hash section, hc step-table row, and pitfall table updated; §12 Cleanup bullet + status snapshot added. `GLOSSARY.md` gains the **simulation setting** term and its `axis`/`ci` entries updated to point at it. Verify these read correctly against the final code

## 8. Templates and example scripts

- [x] 8.1 `inst/targets-templates/large/scenario.R`: change `ci = c(FALSE, TRUE)` to a scalar (`ci = TRUE`); its `partition_by hc = c("ci_method", "est_method")` is unaffected (does not name `"ci"`)
- [x] 8.2 `scripts/example2.R`: change `ci = c(FALSE, TRUE)` to scalar and rewrite the accompanying comment (it currently calls `c(FALSE, TRUE)` "the deliberate way to ask for both") to explain the scalar either/or
- [x] 8.3 Sweep the remaining `scripts/example*.R` for `ci = c(FALSE, TRUE)` / collapse references and update to scalar

## 9. Tests and snapshots

- [x] 9.1 `tests/testthat/test-scenario.R`: replace the "ci = c(FALSE, TRUE) retains the bootstrap axes" test (asserting `s$hc$ci == c(FALSE, TRUE)`) with: `ci = c(FALSE, TRUE)` (and any non-flag) aborts; a scalar `ci = TRUE` stores `s$hc$ci == TRUE` and retains the bootstrap axes; `ci = FALSE` + a bootstrap-only option aborts with the "Set `ci = TRUE`" message. Update the other `ci = c(FALSE, TRUE)` call sites (e.g. line 705) to scalar
- [x] 9.2 `tests/testthat/test-scenario.R` axis-vocabulary assertions (the `task_axes("hc")` / path-vs-inner checks around lines 79, 465–480): drop `"ci"` from the expected hc vocabulary; add a case asserting `partition_by = list(hc = "ci")` is **rejected** as an unknown hc axis (and `bundle = list(hc = "ci")` likewise)
- [x] 9.3 `tests/testthat/test-task-lists.R`: rewrite the "ci = c(FALSE, TRUE) collapses the bootstrap-only scenario options" test (and the `hc_tasks$ci == FALSE` filter) for scalar `ci`: a `ci = FALSE` scenario yields one row per `est_method` with `NA` bootstrap axes; a `ci = TRUE` scenario fans out; assert `!"ci" %in% task_axes("hc")`. Update remaining `ci = c(FALSE, TRUE)` call sites to scalar
- [x] 9.4 `tests/testthat/test-task-shards.R`: update the `ci = c(FALSE, TRUE)` scenario to scalar; assert `ci` is a carried column (present, not a path/inner axis) on the hc shards
- [x] 9.5 `tests/testthat/test-hc-sims.R`: assert `ssd_hc_sims(..., ci = c(FALSE, TRUE))` aborts; assert `est` is byte-identical between `ci = FALSE` and `ci = TRUE`; re-baseline the `hc_sims1` / `hc_sims1ci` snapshots (CI columns shift because `ci` left the primer; `est` unchanged)
- [x] 9.6 `tests/testthat/test-run-scenario.R` and the single-core/targets runner tests: confirm byte-identical results across baseline / sharded / targets for a scalar-`ci` scenario (the cross-runner oracle), and that `ci` round-trips as a carried column through the Parquet shards
- [x] 9.7 Regenerate the affected snapshots: `_snaps/scenario.md` (the three "Set `ci = …`" error messages → "Set `ci = TRUE`"; the scenario-`print` grids that showed `ci = c(FALSE, TRUE)`), `_snaps/task-lists.md` (the `task_axes("hc")` listing that includes `"ci"`; the scenario-`print` and hc-task snapshots), and any hc-task/shard fixtures. Confirm only CI-bearing columns (`se`/`lcl`/`ucl`) and `nboot` move; `est`/`wt`/`dist`/`proportion`/`pboot` stay identical

## 10. Checks

- [x] 10.1 Run `air format .`
- [x] 10.2 Run `devtools::document()` and confirm `NAMESPACE`/`man/` are clean
- [x] 10.3 Run `devtools::test()` (all snapshots intentionally re-baselined) and `devtools::check()`
- [x] 10.4 Run `openspec validate scalar-ci-flag --strict` and confirm it passes
