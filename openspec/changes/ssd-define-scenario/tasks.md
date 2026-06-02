## 1. Input normaliser

- [ ] 1.1 Add `ssd_data()` in `R/data.R`: coerce input to a tibble, assert a `Conc` column exists and is numeric, validate tibble shape; return the normalised tibble
- [ ] 1.2 Add `chk`-based validation with informative error messages for the missing/`non-numeric` `Conc` cases
- [ ] 1.3 Roxygen docs + `@export` for `ssd_data()`

## 2. Scenario constructor

- [ ] 2.1 Add `ssd_define_scenario()` in `R/scenario.R` returning an `ssdsims_scenario` S3 object
- [ ] 2.2 Accept datasets as: single data frame (implicit/explicit name), named list, or unnamed list; forward through `ssd_data()` and derive/accept dataset names per the four-form API
- [ ] 2.3 Implement symbol-capture name derivation (e.g., `rlang::enexpr()`) for implicit cases; fallback to required `name=` for unnamed literals
- [ ] 2.4 Store declarative fields only: `seed`, `nsim`, `nrow`, dataset names (NOT data frames), `fit` grid, `hc` grid, `partition_by`, `upload`
- [ ] 2.5 Populate documented `partition_by` per-step defaults when not supplied; default `upload` to `NULL`
- [ ] 2.6 Validate arguments with `chk` (scalar whole-number `seed`, `nrow` in `[5, 1000]`, `nsim`, etc.); abort on invalid input; error if both named list and `name=` are supplied
- [ ] 2.7 Ensure the constructor performs no RNG draws and leaves `.Random.seed` untouched

## 3. ci = FALSE collapse

- [ ] 3.1 When `ci = FALSE` is the only value, mark `nboot`/`ci_method`/`parametric` as ignored and emit a one-line message
- [ ] 3.2 Record the ignore on the object so `print()` can surface it
- [ ] 3.3 When `ci = c(FALSE, TRUE)`, retain bootstrap knobs and emit no message

## 4. Print method

- [ ] 4.1 Add `print.ssdsims_scenario()` rendering seed, dataset names, `nsim`, `nrow`, and fit/hc grids
- [ ] 4.2 Surface the ignored-knob notice in the print output when present
- [ ] 4.3 Register the S3 method and roxygen docs

## 5. Tests and docs

- [ ] 5.1 `tests/testthat/test-scenario.R`: minimal construction, declarative-only fields, no RNG side effect (`.Random.seed` unchanged)
- [ ] 5.2 Tests for `ssd_data()`: `Conc` required, valid pass-through preserves extra columns
- [ ] 5.3 Tests for dataset input API: single data frame (implicit name + explicit `name=`), named list, unnamed list; symbol capture for common patterns; error on conflict (both named list + `name=`)
- [ ] 5.4 Tests for the `ci = FALSE` rejection (error on bootstrap knobs) and the `ci = c(FALSE, TRUE)` non-rejection
- [ ] 5.5 Snapshot test for `print.ssdsims_scenario()` (with various dataset counts and forms)
- [ ] 5.6 Run `devtools::document()`, `air` formatting, and `devtools::check()`; update `NAMESPACE`/`man/`
