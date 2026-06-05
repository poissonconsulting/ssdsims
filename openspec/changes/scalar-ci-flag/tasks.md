## 1. Constructor: `ci` as a scalar flag

- [ ] 1.1 In `R/scenario.R`, replace the `ci` validation (`chk_logical` + `chk_not_any_na` + `chk_unique` + `chk_length(ci, upper = 2L)`) with `chk::chk_flag(ci)`; keep the default `ci = FALSE`
- [ ] 1.2 Simplify the bootstrap-knob guard from `if (length(ci) == 1L && isFALSE(ci))` to `if (isFALSE(ci))`, and change the message escape hatch from "Set `ci = c(FALSE, TRUE)` to enable bootstrap" to "Set `ci = TRUE` to enable bootstrap"
- [ ] 1.3 Update the `ssd_define_scenario()` roxygen — the *"# `ci = FALSE`"* section — to describe `ci` as a scalar flag (estimate invariant; `ci = TRUE` is the superset; `ci = FALSE` is the cheap, bootstrap-free mode), removing the `c(FALSE, TRUE)` guidance

## 2. Task axes and hc grid: retire the collapse

- [ ] 2.1 In `R/task-lists.R`, remove `"ci"` from `task_axes("hc")` (hc vocabulary becomes `nboot`, `est_method`, `ci_method`, `parametric` on top of the fit axes)
- [ ] 2.2 Rewrite `hc_grid_tbl()` as a single-branch grid keyed by the scalar `scenario$hc$ci`: `ci = FALSE` → one row per `est_method` with `nboot = NA_integer_`, `ci_method = NA_character_`, `parametric = NA`; `ci = TRUE` → `expand_grid` over `nboot × est_method × ci_method × parametric`. Drop the `any(ci == FALSE)` / `any(ci == TRUE)` `bind_rows` branching
- [ ] 2.3 Confirm `ci` is still carried into the hc task row for the runner (`ssdtools::ssd_hc(ci = ...)`) while absent from `task_axes("hc")`, so it never enters `path_key()`, the inner-axis complement, or `task_primer()`

## 3. Low-level entry point

- [ ] 3.1 In `R/hc-sims.R`, add `chk::chk_flag(ci)` to `ssd_hc_sims()` so a vector `ci` is rejected at that layer (it already applies `ci` as a scalar — `ci` is not in its `expand_grid`)

## 4. Tests and snapshots

- [ ] 4.1 `tests/testthat/test-scenario.R`: `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (and any non-flag) aborts in the `ssd_define_scenario()` frame; `ci = FALSE` with a bootstrap knob aborts with the "Set `ci = TRUE`" message; `ci = TRUE` with bootstrap knobs is accepted
- [ ] 4.2 Task-table tests (`test-task-lists.R` / `test-task-shards.R`): `task_axes("hc")` no longer contains `"ci"`; an `hc` task table for `ci = FALSE` has the bootstrap-only knobs `NA` and one row per `est_method`; for `ci = TRUE` it fans out over the bootstrap knobs; assert `ci` is not among the path/inner axes
- [ ] 4.3 `test-hc-sims.R`: `ssd_hc_sims(..., ci = c(FALSE, TRUE))` aborts; re-baseline the `hc_sims1` / `hc_sims1ci` snapshots (CI columns shift because `ci` left the primer; `est` is unchanged — assert byte-identical `est` across `ci = FALSE`/`TRUE`)
- [ ] 4.4 Regenerate affected snapshot fixtures (`_snaps/`) and confirm only CI-bearing columns (`se`/`lcl`/`ucl`) and `nboot` move; `est`/`wt`/`dist`/`proportion`/`pboot` stay identical

## 5. Design doc and checks

- [ ] 5.1 `TARGETS-DESIGN.md`: retire §1.2 (the *ci = FALSE collapse*) — replace with a short note that `ci` is a scalar hc knob (estimate-invariant superset, like `samples`); remove the §1.2 cross-references (the §1 ASCII sidebar bullet, the §5 hc-table row note, the §11 pitfall row, the "§1.2 collapse" mentions)
- [ ] 5.2 `TARGETS-DESIGN.md` §12: add the `scalar-ci-flag` bullet under "Cleanup" (independent tidy-up, off the dependency DAG) and add it to the status-snapshot list of independent tidy-ups
- [ ] 5.3 Run `devtools::document()`, `air format .`; update `man/`
- [ ] 5.4 Run `devtools::test()` and `devtools::check()`
- [ ] 5.5 Run `openspec validate scalar-ci-flag --strict` and confirm it passes
