## 1. Data task table

- [x] 1.1 Add `R/task-lists.R` with a data-task derivation that cross-joins dataset names × `1:nsim` × `replace` (use `tidyr::expand_grid()`); one column per axis
- [x] 1.2 Carry `nrow` as an ordinary column (NOT a cross-join axis); add an inline note pointing to §5 / `nrow-sub-truncation`
- [x] 1.3 Ensure the derivation performs no RNG draws, leaves `.Random.seed` untouched, and adds no `seed`/`primer`/`stream` columns
- [x] 1.4 Roxygen docs + `@export`

## 2. Fit task table

- [x] 2.1 Add a fit-task derivation crossing each data-task identity (`dataset`, `sim`, `replace`) with the scenario's `fit` argument grid (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`)
- [x] 2.2 Carry `min_pmix` by name (not function value); preserve parent-identity columns verbatim for downstream grouping
- [x] 2.3 Roxygen docs + `@export`

## 3. Hc task table

- [x] 3.1 Add an hc-task derivation crossing each fit-task identity with the scenario's `hc` argument grid (`nboot`, `est_method`, `ci_method`, `parametric`)
- [x] 3.2 Honour the scenario's recorded `ci = FALSE` collapse (§1.2): collapse the `ci = FALSE` portion over `nboot`/`ci_method`/`parametric` to a single row instead of the full cross-join
- [x] 3.3 Roxygen docs + `@export`

## 4. Printable task class

- [x] 4.1 Add an `ssdsims_tasks` S3 constructor wrapping a tibble and recording the step (`data`/`fit`/`hc`) as an attribute; have the three derivation functions return it
- [x] 4.2 Add `print.ssdsims_tasks()` rendering step name, cross-join axes, task (row) count, and a compact row preview (parallel `print.ssdsims_scenario()`)
- [x] 4.3 Verify dplyr/tidyr verbs still operate on the object; register the S3 method + roxygen docs

## 5. Baseline runner

- [x] 5.1 Add a runner that executes the three tables in order via `purrr::pmap()` loops: data → fit → hc, threading each step's output into the next
- [x] 5.2 Reuse the existing per-step operations (sim/fit/hc) without RNG seeding; no `targets`, no shard grouping, no `partition_by`, no Parquet I/O
- [x] 5.3 Return the collected per-step results; roxygen docs + `@export`

## 6. Tests and docs

- [x] 6.1 `tests/testthat/test-task-lists.R`: data table has `D * nsim * R` rows; `dataset`/`sim`/`replace` populated; `nrow` present but does not multiply rows
- [x] 6.2 Assert the data derivation is RNG-free (`.Random.seed` unchanged) and has no `seed`/`primer`/`stream` columns
- [x] 6.3 Fit table has `M * F` rows with parent identity + fit-grid columns; `min_pmix` stored by name
- [x] 6.4 Hc table has `K * H` rows; assert the `ci = c(FALSE, TRUE)` example produces the §1.2 reduced fan-out (collapse verified by row count)
- [x] 6.5 Class tests: derived tables inherit from `ssdsims_tasks`, record their step, and survive dplyr/tidyr verbs; snapshot test for `print.ssdsims_tasks()` on each step
- [x] 6.6 Runner test: outputs thread data → fit → hc and collect; assert no `targets` load and no Parquet files written (structure/threading, not draw values)
- [x] 6.7 Snapshot test pinning the three task-table column contracts (so later `task-tables` changes show as diffs)
- [x] 6.8 Run `devtools::document()`, `air format`, and `devtools::check()`; update `NAMESPACE`/`man/`
