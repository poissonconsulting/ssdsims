## 1. Sample task table

- [x] 1.1 Add `R/task-lists.R` with a sample-task derivation that cross-joins dataset names × `1:nsim` × `replace` (use `tidyr::expand_grid()`); one column per axis
- [x] 1.2 Carry the draw size `n_max = max(nrow)` as an ordinary column (NOT a cross-join axis); `nrow` is never a sample axis (§5 / `nrow-sub-truncation`)
- [x] 1.3 Ensure the derivation performs no RNG draws, leaves `.Random.seed` untouched, and adds no `seed`/`primer`/`stream` columns
- [x] 1.4 Roxygen docs + `@export`

## 2. Data (truncation) task table

- [x] 2.1 Add a data-task derivation crossing each sample-task identity (`dataset`, `sim`, `replace`) with the scenario's `nrow` values; each data task is `head(sample, nrow)`
- [x] 2.2 `nrow` is a genuine cross-join axis here (RNG-free truncation), without duplicating the shared draw
- [x] 2.3 Roxygen docs + `@export`

## 3. Fit task table

- [x] 3.1 Add a fit-task derivation crossing each data-task identity (`dataset`, `sim`, `replace`, `nrow`) with the scenario's `fit` argument grid (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`)
- [x] 3.2 Carry `min_pmix` by name (not function value); preserve parent-identity columns verbatim for downstream grouping
- [x] 3.3 Roxygen docs + `@export`

## 4. Hc task table

- [x] 4.1 Add an hc-task derivation crossing each fit-task identity with the scenario's `hc` argument grid (`nboot`, `est_method`, `ci_method`, `parametric`)
- [x] 4.2 Honour the scenario's recorded `ci = FALSE` collapse (§1.2): collapse the `ci = FALSE` portion over `nboot`/`ci_method`/`parametric` to a single row instead of the full cross-join
- [x] 4.3 Roxygen docs + `@export`

## 5. Task ids and foreign keys

- [x] 5.1 Add a path-style `<step>_id` primary key per table (the Hive partition path, §5/§6) built from the step's cross-join axes
- [x] 5.2 Carry each non-root step's parent id (`sample_id`/`data_id`/`fit_id`) as an explicit foreign-key column so dependencies are a single joinable column
- [x] 5.3 Tests: ids are unique path-style keys; every foreign key resolves to a parent primary key

## 6. Printable task class and compound expansion

- [x] 6.1 Add an `ssdsims_tasks` S3 constructor wrapping a tibble and recording the step (`sample`/`data`/`fit`/`hc`) as an attribute; have the four derivation functions return it
- [x] 6.2 Add `print.ssdsims_tasks()` rendering step name, cross-join axes, task (row) count, and a compact row preview (parallel `print.ssdsims_scenario()`)
- [x] 6.3 Verify dplyr/tidyr verbs still operate on the object; register the S3 method + roxygen docs
- [x] 6.4 Add `ssd_scenario_tasks()` calling the four derivations and returning an `ssdsims_task_set` exposing `sample`/`data`/`fit`/`hc` (the canonical expansion entry point, §1/§2)
- [x] 6.5 Add `print.ssdsims_task_set()` summarising per-step task counts; roxygen docs + `@export`

## 7. Baseline runner

- [x] 7.1 Add a runner that loops over the four derived tables via `purrr::pmap()`: sample → data → fit → hc (it does NOT expand tasks itself; it consumes `ssd_scenario_tasks()`)
- [x] 7.2 Thread each step's output to the next by looking up the parent result via the parent's id foreign key; reuse the per-step ops (sample/truncate/fit/hc) without RNG seeding; no `targets`, no shard grouping, no `partition_by`, no Parquet I/O
- [x] 7.3 Return the collected per-step results; roxygen docs + `@export`

## 7b. Scenario extensions (reviewer-driven, #80)

- [x] 7b.1 Add a validated `replace` knob to `ssd_define_scenario()`; the `sample` axis reads `scenario$replace`
- [x] 7b.2 Retain the validated `ssd_data()` collection on the scenario (`$data`); `ssd_run_scenario_baseline(scenario)` reads it and drops its `data` argument
- [x] 7b.3 Prefer purrr/rlang over base (recorded in CLAUDE.md); build the task tibble with `tibble::new_tibble()`; define "axis" in GLOSSARY.md

## 8. Tests and docs

- [x] 8.1 `tests/testthat/test-task-lists.R`: sample table has `D * nsim * R` rows; `dataset`/`sim`/`replace` populated; `n_max` carried and `nrow` does not multiply the draw
- [x] 8.2 Assert the sample derivation is RNG-free (`.Random.seed` unchanged) and has no `seed`/`primer`/`stream` columns
- [x] 8.3 Data table has `S * |nrow|` rows with `nrow` as a genuine axis; fit table has `M * F` rows with parent identity + fit-grid columns; `min_pmix` stored by name
- [x] 8.4 Hc table has `K * H` rows; assert the `ci = c(FALSE, TRUE)` example produces the §1.2 reduced fan-out (collapse verified by row count)
- [x] 8.5 Id/foreign-key tests; class tests (inherit `ssdsims_tasks`, record step, survive dplyr/tidyr verbs); snapshot tests for `print.ssdsims_tasks()` and `print.ssdsims_task_set()`
- [x] 8.6 Runner test: outputs thread sample → data → fit → hc and collect; truncation is `head(sample, nrow)`; assert no `targets` load and no Parquet files written (structure/threading, not draw values)
- [x] 8.7 Snapshot test pinning the four task-table column contracts (so later `task-tables` changes show as diffs)
- [x] 8.8 Run `devtools::document()`, `air format`, and `devtools::check()`; update `NAMESPACE`/`man/`
