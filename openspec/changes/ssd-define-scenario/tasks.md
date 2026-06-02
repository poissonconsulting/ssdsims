## 1. Input normaliser

- [x] 1.1 Add `ssd_data()` in `R/data.R`: coerce input to a tibble, assert a `Conc` column exists and is numeric, validate tibble shape; return the normalised tibble
- [x] 1.2 Add `chk`-based validation with informative error messages for the missing/`non-numeric` `Conc` cases
- [x] 1.3 Roxygen docs + `@export` for `ssd_data()`

## 2. Scenario constructor

- [x] 2.1 Add `ssd_define_scenario()` in `R/scenario.R` returning an `ssdsims_scenario` S3 object
- [x] 2.2 Accept datasets as: single data frame (implicit/explicit name), named list, or unnamed list; forward through `ssd_data()` and derive/accept dataset names per the four-form API
- [x] 2.3 Implement symbol-capture name derivation (e.g., `rlang::enexpr()`) for implicit cases; fallback to required `name=` for unnamed literals
- [x] 2.4 Store declarative fields only: `seed`, `nsim`, `nrow`, dataset names (NOT data frames), `fit` grid, `hc` grid, `partition_by`, `upload`
- [x] 2.4a Store `min_pmix` in the `fit` grid **by name** (NOT as a function value): accept a character vector of names, or a function / list of functions whose name is derived by symbol capture; validate provided functions before taking the name. Registry resolution is deferred to `min-pmix-registry`.
- [x] 2.5 Populate documented `partition_by` per-step defaults when not supplied; default `upload` to `NULL`
- [x] 2.6 Validate arguments with `chk` (scalar whole-number `seed`, `nrow` in `[5, 1000]`, `nsim`, etc.); abort on invalid input; error if both named list and `name=` are supplied
- [x] 2.7 Ensure the constructor performs no RNG draws and leaves `.Random.seed` untouched

## 3. ci = FALSE rejects bootstrap-only knobs

- [x] 3.1 When `ci = FALSE` is the only value, abort with an informative error if any bootstrap-only knob (`nboot`/`ci_method`/`parametric`) was explicitly passed
- [x] 3.2 Error message names the offending knob(s) and points to `ci = c(FALSE, TRUE)` as the way to enable bootstrap
- [x] 3.3 When `ci = c(FALSE, TRUE)`, retain bootstrap knobs and emit no error

## 4. Print method

- [x] 4.1 Add `print.ssdsims_scenario()` rendering seed, dataset names, `nsim`, `nrow`, and fit/hc grids
- [x] 4.2 Register the S3 method and roxygen docs

## 5. Tests and docs

- [x] 5.1 `tests/testthat/test-scenario.R`: minimal construction, declarative-only fields, no RNG side effect (`.Random.seed` unchanged)
- [x] 5.2 Tests for `ssd_data()`: `Conc` required, valid pass-through preserves extra columns
- [x] 5.3 Tests for dataset input API: single data frame (implicit name + explicit `name=`), named list, unnamed list; symbol capture for common patterns; error on conflict (both named list + `name=`)
- [x] 5.3a Tests for `min_pmix` by name: stored as character (default derives `"ssd_min_pmix"`), names accepted verbatim, function / named-list / unnamed-list derivation, non-function list element rejected
- [x] 5.4 Tests for the `ci = FALSE` rejection (error on bootstrap knobs) and the `ci = c(FALSE, TRUE)` non-rejection
- [x] 5.5 Snapshot test for `print.ssdsims_scenario()` (with various dataset counts and forms)
- [x] 5.6 Run `devtools::document()`, `air` formatting, and `devtools::check()`; update `NAMESPACE`/`man/`
