## 1. `nrow_max` setting in the constructor (`R/scenario.R`, `R/params.R`)

- [x] 1.1 Add `nrow_max` to `ssd_define_scenario()` in the contiguous simulation-settings block (after `parametric`, with `proportion`/`ci`/`samples`); `chk::chk_whole_number(nrow_max)`, default `1000L`; store it on the scenario (decide top-level `scenario$nrow_max` vs `scenario$sample$nrow_max` — see design Open Questions).
- [x] 1.2 Move/extend `nrow` validation to check each `nrow` against the effective draw size: `nrow <= nrow(data)` for `replace = FALSE`, `nrow <= nrow_max` for `replace = TRUE`; abort in the user-facing frame (`call = environment()`), keeping the existing `[5, 1000]` range check.
- [x] 1.3 Add the `@param nrow_max` roxygen in `R/params.R`; have `print.ssdsims_scenario()` render `nrow_max` among the simulation settings.

## 2. Drop the carried columns from the task tables (`R/task-lists.R`)

- [x] 2.1 `sample_task_grid()`: remove `grid$n_max <- max(scenario$nrow)` — the sample table carries only `(dataset, sim, replace)`.
- [x] 2.2 `hc_grid_tbl()`: stop emitting the `ci` column in both branches; keep the `ci = FALSE ⟹ nboot/ci_method/parametric = NA` canonicalisation, keyed off the scalar `scenario$hc$ci`.
- [x] 2.3 Baseline runner (`ssd_run_scenario_baseline()`): compute the per-task draw size `D = min(nrow_max, nrow(data))` for `replace = FALSE` / `nrow_max` for `replace = TRUE` from `scenario` + the dataset (was `t$n_max`); read `ci` from `scenario$hc$ci` (was the `ci` column / `hc_args`), as it already reads `proportion`/`samples`/`dists`.

## 3. Slice and shard runners (`R/targets-runner.R`)

- [x] 3.1 `scenario_step_slice()`: carry `nrow_max` on the `sample` slice and `ci` on the `hc` slice (alongside `proportion`/`samples`).
- [x] 3.2 `ssd_run_sample_step()`: compute the draw size from the slice's `nrow_max` + the dataset (`nrow(scenario_dataset(...))`) instead of reading `t$n_max`.
- [x] 3.3 `ssd_run_hc_step()`: read `ci` from the slice (`scenario$hc$ci`) instead of `t$ci`.
- [x] 3.4 Confirm no primer/partition path reads `t$n_max`/`t$ci` (neither is in `task_axes()`), so `task_primer()`/`path_key()`/`shard_path()` are untouched.

## 4. Docs and design

- [x] 4.1 Update `TARGETS-DESIGN.md` §5 prose (the `n_max = max(scenario$nrow)` derivation → the fixed `nrow_max` draw and the retired re-draw churn). (`ROADMAP.md` entry already added at propose.)
- [x] 4.2 `GLOSSARY.md`: rework the `n_max` carried-column entry (it is no longer a row column) and add a `nrow_max` entry (a simulation setting; the fixed draw size).
- [x] 4.3 `devtools::document()`; regenerate `man/`; sweep example scripts / `inst/targets-templates/` for any reliance on the derived draw size, and add `nrow_max` where illustrative.

## 5. Tests and snapshots

- [x] 5.1 `test-scenario.R`: `nrow_max` defaults to `1000L`, validates as a whole number, and rejects `nrow` exceeding the effective draw size (per `replace`); `nrow_max` absent from `task_axes("sample")`.
- [x] 5.2 `test-task-lists.R`: the sample table has no `n_max` column and the hc table has no `ci` column; the `ci = FALSE` NA-canonicalisation still holds; baseline-runner results match the pre-change values only where the draw is unchanged (re-baseline otherwise).
- [x] 5.3 `test-parallel-safe-seeding` / sub-truncation: `head(., n)` is a prefix of the fixed-`D` draw; adding an `nrow` within `D` yields a byte-identical draw for a fixed seed.
- [x] 5.4 `test-task-shards` (or the slice tests): the `sample` slice carries `nrow_max`, the `hc` slice carries `ci`; extending `nrow` within the draw size caches the `sample` shard; changing `nrow_max` rebuilds it.
- [x] 5.5 Re-baseline all affected snapshots (`_snaps/scenario.md`, `_snaps/task-lists.md`, `_snaps/hc-sims.md`, and any sample-draw fixtures) and review the diffs.

## 6. Finalise

- [x] 6.1 `air format .`, then `devtools::test()` / `R CMD check`; fix fallout.
  (`devtools::test()`: 668 pass. The only 3 failures are pre-existing
  `tar_make()` "could not find function `tar_map`/`tar_option_set`" errors in
  the persistent `targets_session()` callr worker — reproduced identically on
  the base branch, an environment libpath quirk unrelated to this change.)
- [x] 6.2 `openspec validate nrow-max-setting --strict` passes.
