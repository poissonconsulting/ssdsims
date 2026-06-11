## 1. Default flip (`R/scenario.R`, `R/params.R`)

- [x] 1.1 Change the `ssd_define_scenario()` signature default `replace = FALSE` → `replace = TRUE`; confirm the existing effective-draw-size validation already covers both branches (no logic change).
- [x] 1.2 Note the `TRUE` default (and the permutation alternative) in `@param replace` — as a scenario-specific `@param` override in `R/scenario.R`, since the shared `R/params.R` entry is inherited by the legacy `ssd_sim_data()`/`ssd_run_scenario()` whose default stays `FALSE`; `GLOSSARY.md`: note the default in the `replace` entry.

## 2. Tests

- [x] 2.1 Re-point default-`replace` expectations: `unique(tasks$replace)` is `TRUE`; `sample_id`/path-key strings become `replace=TRUE`; default-scenario draw size is `nrow_max` rows.
- [x] 2.2 Make tests whose point is the `replace = FALSE` permutation cap explicit (`replace = FALSE` in the call), including the `nrow`-exceeds-`nrow(data)` validation snapshot and the full-permutation baseline-draw test.
- [x] 2.3 Add mixed-`replace` tests: `replace = c(FALSE, TRUE)` with `nrow` above the dataset size aborts (snapshot); with two datasets and an `nrow` exceeding only the smaller one, the error names that dataset; default-`replace` accepts `nrow > nrow(data)` within `nrow_max`.
- [x] 2.4 Re-record affected snapshots (`_snaps/scenario.md` prints, `_snaps/task-lists.md` tables/ids).

## 3. Docs and finalise

- [x] 3.1 `devtools::document()`; sweep vignettes/templates for prose assuming the `FALSE` default; re-render the vignettes.
- [x] 3.2 `air format .`; full `devtools::test()`; `openspec validate replace-default-true --strict`.
