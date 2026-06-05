## 1. Public `as_ssd_data()` coercer

- [ ] 1.1 Add `as_ssd_data(data, name = NULL, call = rlang::current_env())` in `R/data.R` (next to `ssd_data()` / `ssd_data_validate()`): coerce the already-named forms into a validated `ssdsims_data` collection — (a) an `ssd_data()` collection passthrough (return unchanged, abort if a redundant `name=` is supplied), (b) a named list of data frames (names from the list; abort if `name=` is also supplied; reuse the `anyDuplicated()` duplicate-name check), (c) a single data frame with an explicit `name=` (wrap under that name)
- [ ] 1.2 Validate every element through the existing `ssd_data_validate(data, name = ..., call = call)` so the numeric-`Conc` contract is identical to `ssd_data()` (no duplication)
- [ ] 1.3 Reject a bare data frame with no `name=` and an unnamed list with an informative `chk::abort_chk()` message pointing the caller at `ssd_define_scenario()` (which captures the argument expression) or at supplying names via a named list / `name=`; do **not** attempt symbol capture in `as_ssd_data()`
- [ ] 1.4 Add a `call` argument (defaulting to `as_ssd_data()`'s own frame) so delegated validation errors can surface in the caller's frame per the error-call-origin discipline in `R/scenario.R`

## 2. Export and document

- [ ] 2.1 Add roxygen `@export` (plus `@param` / `@return` / `@examples`) to `as_ssd_data()`; cross-reference `ssd_data()` and `ssd_define_scenario()`
- [ ] 2.2 Add `as_ssd_data` to the `_pkgdown.yml` reference group alongside `ssd_data`

## 3. Refactor `scenario_datasets()` to delegate

- [ ] 3.1 In `R/scenario.R`, refactor `scenario_datasets()` to keep only the **symbol-capture** name derivation in the constructor frame: collection passthrough delegates to `as_ssd_data(data, name = name, call = call)`; a bare data frame derives `name` via `expr_to_name(data_expr)` (keeping the existing "supply an explicit `name=` or use `ssd_data()`" abort) then delegates; a named list delegates with `name` forwarded; an unnamed list derives per-element names via `list_expr_names(data_expr, ...)`, attaches them, then delegates the now-named list
- [ ] 3.2 Move the shared `name`-conflict and duplicate-name messages into `as_ssd_data()` where they apply uniformly; keep `expr_to_name()` / `list_expr_names()` untouched
- [ ] 3.3 Forward the constructor's `call` through delegation so validation errors continue to surface as `ssd_define_scenario()`

## 4. Tests

- [ ] 4.1 `tests/testthat/test-data.R`: `as_ssd_data()` returns an `ssd_data()` collection unchanged; aborts on a redundant `name=` with a collection
- [ ] 4.2 A named list of data frames coerces to an `ssdsims_data` collection using the list names, each element validated and coerced to a tibble; a single data frame with `name=` coerces to a one-element named collection
- [ ] 4.3 A bare data frame with no `name=` and an unnamed list each abort with the actionable message naming `ssd_define_scenario()` and the named-list / `name=` escape hatches; duplicate names abort; a non-numeric `Conc` aborts via `ssd_data_validate()`
- [ ] 4.4 `tests/testthat/test-scenario.R`: every `ssd_define_scenario()` input form still produces the same dataset names and `Conc` validation as before (symbol-capture forms — `ssddata::ccme_boron`, `list(ssddata::ccme_boron, ssddata::ccme_cadmium)` — yield the same names; errors surface in the `ssd_define_scenario()` frame)

## 5. Docs and checks

- [ ] 5.1 Run `devtools::document()`, `air format .`, and `devtools::check()`; update `NAMESPACE` (`export(as_ssd_data)`) and `man/`
- [ ] 5.2 Run `openspec validate cleanup-as-ssd-data --strict` and confirm it passes
