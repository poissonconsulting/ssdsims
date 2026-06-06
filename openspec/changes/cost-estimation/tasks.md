## 1. Calibration object and accessor

- [ ] 1.1 Define the `ssdsims_cost_calibration` S3 object in `R/cost-estimate.R`: per-`ci_method` coefficient table (`base`, `slope`, `n0`), `nrow_factor` table, `fixed_addend`, and `provenance` (cpu, R version, ssdtools version, date, sweep grid).
- [ ] 1.2 Add `print`/`format` methods rendering coefficients, provenance, and a ballpark caveat.
- [ ] 1.3 Add `ssd_cost_calibration()` accessor returning the shipped default object.

## 2. Calibration harness

- [ ] 2.1 Implement `ssd_calibrate_cost()`: fit one bcanz model per `nrow` in a small default set (resampling `ssddata::ccme_boron`), time `ssdtools::ssd_hc()` over tiny `nboot` × all `ssd_ci_methods()`, fit `lm(time ~ pmax(nboot, n0))` per `ci_method` (grid-search `n0`), and derive the bounded `nrow_factor`.
- [ ] 2.2 Capture provenance (CPU from `/proc/cpuinfo` or `benchmarkme`-free fallback, `R.version.string`, `ssdtools` version, date, sweep grid) into the returned `ssdsims_cost_calibration`.
- [ ] 2.3 Validate inputs with `chk` and keep the sweep self-contained (no new dependencies; base `system.time`).

## 3. Estimator

- [ ] 3.1 Implement `ssd_estimate_cost(scenario, calibration = ssd_cost_calibration())`: expand via `ssd_scenario_hc_tasks()` read-only (no fit/bootstrap/RNG), apply the per-task model per row.
- [ ] 3.2 Return total serial cost and longest single-task duration as time quantities, plus a per-axis breakdown grouped by `ci_method × nboot`; annotate with the calibration's provenance and caveat.
- [ ] 3.3 Ensure `proportion`/`est_method` do not multiply the estimate and that the longest task reflects the costliest cell (per slopes), not the most numerous.

## 4. Shipped default calibration data

- [ ] 4.1 Add a `data-raw/cost_calibration.R` script that runs `ssd_calibrate_cost()` and saves the object; record this session's coefficients (weighted_samples 5.68 / GMACL 18.90 / MACL 19.64 / arithmetic_samples 21.24 / geometric_samples 21.60 / multi_fixed 52.63 / multi_free 55.00 ms/boot; n0 ≈ 25–30) and provenance (Intel Xeon @ 2.10 GHz, R 4.5.3, ssdtools 2.6.0.9002).
- [ ] 4.2 Check in `data/ssd_cost_calibration_default.rda`; document the dataset (`R/data.R`) and add to `_pkgdown.yml`.

## 5. Reproducible analysis vignette

- [ ] 5.1 Write `vignettes/cost-estimation.qmd` documenting the method (calibration sweep, `max(nboot, n0)` per-`ci_method` model, free `proportion`/`est_method`, bounded non-monotonic `nrow` factor) and outcome (estimator on the motivating ~430 h scenario).
- [ ] 5.2 Make the vignette rerun `ssd_calibrate_cost()` end-to-end so it yields a custom estimator for the rendering architecture; add to `_pkgdown.yml`.

## 6. Tests, docs, checks

- [ ] 6.1 Tests: estimation runs no RNG/fit and writes nothing (`.Random.seed` unchanged); free axes don't change the total; longest task tracks the costliest `multi_*` × max-`nboot` cell; `max(nboot, n0)` floor behaviour.
- [ ] 6.2 `devtools::document()`, add reference-index entries, run `pkgdown::check_pkgdown()`.
- [ ] 6.3 Run `air format .`, `devtools::test()`, `devtools::check()`.
