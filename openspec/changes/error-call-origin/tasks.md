## 1. Audit baseline and reference pattern

- [ ] 1.1 Re-grep the exported surface for leaked origins: `chk_all`, `purrr::walk`, `chk::chk_` without a threaded `call`, and `abort_chk` without `call =`, across `R/` (excluding the already-compliant `R/data.R` / `R/scenario.R`); record one audit row per exported function
- [ ] 1.2 Confirm the reference pattern in `R/data.R` (`ssd_data()` → `ssd_data_names()` / `ssd_data_validate()` with `call <- environment()` and `call = rlang::caller_env()`) and `R/scenario.R`, and apply it unchanged to the functions below

## 2. `R/fit-dists-sims.R` — replace `chk_all`, thread `call`

- [ ] 2.1 Capture `call <- environment()` at the top of `ssd_fit_dists_sims()` and pass `call = call` to its `chk::chk_*()` / `chk::abort_chk()` validators
- [ ] 2.2 Replace `chk::chk_all(min_pmix, chk::chk_function, formals = 1L)` with a plain `for` loop that validates each element with the public `call` (guard + `chk::abort_chk(..., call = call)` where the element check has no `call` argument)
- [ ] 2.3 Replace the `chk::chk_all(range_shape1, ...)` and `chk::chk_all(range_shape2, ...)` element checks with plain `for` loops threading the public `call`

## 3. `R/hc-sims.R` and `R/simulate-data.R`

- [ ] 3.1 `ssd_hc_sims()`: capture `call <- environment()` and thread `call = call` into its validators and any private helper; replace any `chk_all` / `purrr::walk` on a validation path with a plain loop
- [ ] 3.2 `ssd_sim_data()`: capture and thread the public frame into its validators and helpers so a bad argument names `ssd_sim_data()`

## 4. Scenario task/shard builders and accessors

- [ ] 4.1 `R/task-lists.R` / `R/task-shards.R`: thread the public frame through `ssd_scenario_sample_tasks()` / `ssd_scenario_fit_tasks()` / `ssd_scenario_hc_tasks()` / `ssd_scenario_tasks()` and the `*_shards()` builders so a failure names the exported function, not the shared private helper (e.g. `scenario_shards()`)
- [ ] 4.2 `R/accessors.R`: thread `call = environment()` into `scenario_dataset()` / `scenario_min_pmix()` / `scenario_results_dir()` validators

## 5. Runners and RNG/seed helpers

- [ ] 5.1 `R/run-scenario.R`: thread the public frame through `ssd_run_scenario()` / `ssd_run_scenario_baseline()` / `ssd_run_scenario_shards()`
- [ ] 5.2 `R/targets-runner.R` / `R/shard-runner.R`: thread the public frame through `ssd_run_sample_step()` / `ssd_run_fit_step()` / `ssd_run_hc_step()` / `ssd_scenario_targets()`
- [ ] 5.3 `R/task-primer.R`: thread the public frame through `task_primer()` validators
- [ ] 5.4 `R/dqrng-state.R` / `R/lecuyer-cmrg-seed.R`: thread the public frame through the exported `with_*` / `local_*` helpers' validators

## 6. Regression tests

- [ ] 6.1 Add per-function origin tests: trigger a validation failure and assert the origin with `testthat::expect_error(..., class = ...)` plus `testthat::expect_snapshot()` of the rendered error, asserting the `Error in \`ssd_*()\`:` header names the public function and not `chk_all` / `purrr` / a helper
- [ ] 6.2 Cover `ssd_fit_dists_sims()` (the `chk_all` → loop cases on `min_pmix` / `range_shape*`) explicitly, pinning the new per-element message wording in a snapshot
- [ ] 6.3 Add a cross-cutting test iterating the exported surface (or one test per function) asserting no validation error reports an internal frame as origin

## 7. Document, format, and validate

- [ ] 7.1 Run `devtools::document()` and `air format .`; update `man/` / `NAMESPACE` if any signature gained a `call =` argument
- [ ] 7.2 Run `devtools::check()` (and accept the new snapshots) — no NOTEs/WARNINGs introduced
- [ ] 7.3 Run `openspec validate error-call-origin --strict` and confirm it passes
