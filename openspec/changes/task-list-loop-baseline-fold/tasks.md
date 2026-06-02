## 1. Task model (R/task-lists.R)

- [x] 1.1 Update `task_axes()`: drop the `data` case; `fit = c("dataset", "sim", "replace", "nrow", "rescale", "computable", "at_boundary_ok", "min_pmix", "range_shape1", "range_shape2")`; `hc = c(fit, "ci", "nboot", "est_method", "ci_method", "parametric")`
- [x] 1.2 Update `task_parent()`: `fit`'s parent is `"sample"`; `hc`'s parent stays `"fit"`; `sample` remains the root; remove the `data` entry
- [x] 1.3 Replace `fit_task_table()` to cross the sample grid (carrying `nrow` via `expand_grid` over `scenario$nrow`) with `fit_grid_tbl()`; remove `ssd_scenario_data_tasks()` and `data_task_grid()`

## 2. Runner

- [x] 2.1 Remove the standalone `data` loop from `ssd_run_scenario_baseline()`; in the `fit` step resolve the parent draw by `sample_id` and fit `utils::head(sample_out[[sample_id]], nrow)`
- [x] 2.2 Update `ssd_scenario_tasks()` and the `ssdsims_task_set` to expose `sample`/`fit`/`hc` (three tables); update `print.ssdsims_task_set()` to loop over the three steps

## 3. Docs

- [x] 3.1 Update roxygen: remove `ssd_scenario_data_tasks()`; update `ssd_scenario_fit_tasks()` (crosses sample identity × `nrow` × fit grid, truncates inline), `ssd_scenario_tasks()` (three tables), and the runner; run `devtools::document()` to refresh `man/`/`NAMESPACE` and `_pkgdown.yml`
- [x] 3.2 In `TARGETS-DESIGN.md` §12, mention that the already-applied `task-list-loop-baseline` change was expanded (the `data` truncation folded into `fit`) — no new DAG node; annotate the §5 step table to three steps

## 4. Tests and snapshots

- [x] 4.1 Update `tests/testthat/test-task-lists.R`: remove `data`-table tests; assert `fit` row count is `S * |nrow| * F`, `fit` carries `nrow` + `sample_id` (not `data_id`), and `fit` truncates inline (`head`); `hc` parent is `fit`
- [x] 4.2 Assert the §5 sub-truncation property still holds (a fit on `nrow = 5` sees the byte-identical prefix of the `nrow = 10` draw) and the `sample` derivation is unchanged/RNG-free
- [x] 4.3 Regenerate `_snaps/task-lists.md` (drop `data`; update task counts/previews and the task-set print) and review the diff

## 5. Checks

- [x] 5.1 Run `devtools::test(filter = "task-lists")`, `air format .`, and `devtools::check()`; confirm 0 errors/0 warnings and `pkgdown::check_pkgdown()` clean
