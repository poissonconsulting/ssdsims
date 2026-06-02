## 1. Input normaliser

- [ ] 1.1 Add `ssd_data()` in `R/data.R`: coerce input to a tibble, assert a `Conc` column exists and is numeric, validate tibble shape; return the normalised tibble
- [ ] 1.2 Add `chk`-based validation with informative error messages for the missing/`non-numeric` `Conc` cases
- [ ] 1.3 Roxygen docs + `@export` for `ssd_data()`

## 2. Scenario constructor

- [ ] 2.1 Add `ssd_define_scenario()` in `R/scenario.R` returning an `ssdsims_scenario` S3 object
- [ ] 2.2 Forward input data through `ssd_data()` and derive/accept the dataset name(s)
- [ ] 2.3 Store declarative fields only: `seed`, `nsim`, `nrow`, dataset names, `fit` grid, `hc` grid, `partition_by`, `upload`
- [ ] 2.4 Populate documented `partition_by` per-step defaults when not supplied; default `upload` to `NULL`
- [ ] 2.5 Validate arguments with `chk` (scalar whole-number `seed`, `nrow` in `[5, 1000]`, `nsim`, etc.); abort on invalid input
- [ ] 2.6 Ensure the constructor performs no RNG draws and leaves `.Random.seed` untouched

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
- [ ] 5.3 Tests for the `ci = FALSE` collapse (message + recorded ignore) and the `ci = c(FALSE, TRUE)` non-collapse
- [ ] 5.4 Snapshot test for `print.ssdsims_scenario()` (with and without ignored knobs)
- [ ] 5.5 Run `devtools::document()`, `air` formatting, and `devtools::check()`; update `NAMESPACE`/`man/`
