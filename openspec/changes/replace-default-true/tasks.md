## 1. Default flip (`R/scenario.R`, `R/params.R`)

- [ ] 1.1 Change the `ssd_define_scenario()` signature default `replace = FALSE` → `replace = TRUE`.
- [ ] 1.2 Note the `TRUE` default (and the permutation alternative) in `@param replace` — as a scenario-specific `@param` override in `R/scenario.R`, since the shared `R/params.R` entry is inherited by the legacy `ssd_sim_data()`/`ssd_run_scenario()` whose default stays `FALSE`; `GLOSSARY.md`: note the default in the `replace` entry.

## 2. Discard infeasible `replace = FALSE` cells (`R/scenario.R`, `R/task-lists.R`)

- [ ] 2.1 Relax the per-dataset `replace = FALSE` construction abort (introduced by `nrow-max-setting`) so it no longer poisons the scenario; keep the `replace = TRUE` / `nrow > nrow_max` abort in the user-facing frame.
- [ ] 2.2 In `R/task-lists.R`, drop the infeasible cells when materialising the grid: in `fit_task_table()` (and the `sample` grid) remove rows where `replace == FALSE` and `nrow > min(nrow_max, nrow(data))` for that dataset; keep all `replace = TRUE` cells and all feasible `replace = FALSE` cells. Drop a `(dataset, replace = FALSE)` `sample` cell only when no `nrow` survives for it.
- [ ] 2.3 Add the empty-grid guard: if the discard leaves no feasible task at all, abort construction in the user-facing frame.

## 3. Tests

- [ ] 3.1 Re-point default-`replace` expectations: `unique(tasks$replace)` is `TRUE`; `sample_id`/path-key strings become `replace=TRUE`; default-scenario draw size is `nrow_max` rows.
- [ ] 3.2 Make tests whose point is the `replace = FALSE` permutation cap explicit (`replace = FALSE` in the call), including the full-permutation baseline-draw test.
- [ ] 3.3 Add discard tests: `replace = c(FALSE, TRUE)` with `nrow` above the dataset size keeps the `replace = TRUE` cells and drops the `replace = FALSE` ones (task-count / `replace`-value assertions, no warning); with two datasets and an `nrow` exceeding only the smaller one, only that dataset's `replace = FALSE` cell is dropped; `replace = FALSE`-only with every `nrow` infeasible aborts (snapshot); default-`replace` accepts `nrow > nrow(data)` within `nrow_max`.
- [ ] 3.4 Re-record affected snapshots (`_snaps/scenario.md` prints, `_snaps/task-lists.md` tables/ids).

## 4. Docs and finalise

- [ ] 4.1 `devtools::document()`; sweep vignettes/templates for prose assuming the `FALSE` default or the rectangular-grid abort; re-render the vignettes.
- [ ] 4.2 `air format .`; full `devtools::test()`; `openspec validate replace-default-true --strict`.
