## 1. Axis vocabulary

- [ ] 1.1 Remove `"est_method"` from `task_axes("hc")` in `R/task-lists.R` (hc axes become `nboot`, `ci_method`, `parametric`).
- [ ] 1.2 Update the hc task-table derivation so the `ci = TRUE` fan-out is `nboot × ci_method × parametric` and `ci = FALSE` yields exactly one hc row per fit task (no `est_method` fan-out); carry `est_method` as a setting, not an identity column.
- [ ] 1.3 Confirm `partition_by`/`bundle` validation rejects `"est_method"` for the `hc` step (falls out of `task_axes("hc")` automatically); add/adjust the bespoke-vs-generic error path if needed.

## 2. Single-bootstrap hc computation

- [ ] 2.1 In `R/hc-sims.R`, drop `est_method` from the `expand_grid(...)` cross-join; keep `est_method` as a scalar/vector setting argument.
- [ ] 2.2 In `R/internal.R` (`hc_state()`/`hc_seed()`), run the bootstrap once per `(nboot, ci_method, parametric)` cell with `samples = TRUE` (using the first requested `est_method`) to obtain the est_method-invariant `se`/`lcl`/`ucl` (+ `samples`).
- [ ] 2.3 Derive each requested `est_method`'s analytical point `est` via a bootstrap-free `ssd_hc(fit, ci = FALSE, est_method = m)` call, and assemble one output row per method (its `est` joined to the shared CI columns), preserving requested-method order and the existing `hc` tibble column set.
- [ ] 2.4 Ensure `ci = FALSE` path still returns one analytical row per `est_method` with no bootstrap.

## 3. Scenario constructor and storage

- [ ] 3.1 In `R/scenario.R`, move `est_method` out of the hc-axis block into the contiguous simulation-settings block (order: `est_method`, `proportion`, `ci`, `samples`); keep validation (character, non-`NA`, unique, subset of `ssdtools::ssd_est_methods()`, length ≥ 1).
- [ ] 3.2 Store `est_method` at `scenario$hc$est_method` as a setting and render it with the hc settings in `print.ssdsims_scenario()` (axes `nboot`/`ci_method`/`parametric` first, then `est_method`/`proportion`/`ci`/`samples`).

## 4. Correctness gate

- [ ] 4.1 Add a regression test asserting byte-identity (`est`, `se`, `lcl`, `ucl`, and `samples` when retained) between the collapsed `ssd_hc_sims()` and the prior per-method path, across every `est_method × ci_method` and both `ci` values, at a fixed seed.
- [ ] 4.2 Add a test that a vector `est_method` does not multiply hc tasks/rows (row count independent of the number of methods; `hc` tibble still covers all methods).

## 5. Call sites, docs, snapshots

- [ ] 5.1 Sweep call sites (examples in `@examples`, `tests/`, `scripts/`, `vignettes/`, `inst/targets-templates/`) to pass `est_method` in the new signature position.
- [ ] 5.2 Update roxygen docs and run `devtools::document()`; update `GLOSSARY.md`/`TARGETS-DESIGN.md` references that list `est_method` as an hc axis.
- [ ] 5.3 Re-record affected snapshots (printed scenarios, hc task-count assertions).
- [ ] 5.4 Run `air format .`, `devtools::test()`, and `devtools::check()`; verify the est_method axis is ~3× cheaper on a small `ci = TRUE` scenario.
