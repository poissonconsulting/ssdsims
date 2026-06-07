## 1. Validation guard

- [x] 1.1 Add a `ci` parameter to `validate_partition_by()` (`R/scenario.R`) and thread it through to `validate_axis_list()`; pass `ci` at the `validate_partition_by()` call site.
- [x] 1.2 In `validate_axis_list()`, after the existing generic `task_axes(step)` subset check, add an `isFALSE(ci)` branch that, for `step == "hc"`, rejects any axis in `c("nboot", "ci_method", "parametric")` — aborting via `chk::abort_chk(..., call = call)` (plain loop, no `purrr::walk`), naming the offending axis/axes and directing the user to set `ci = TRUE` or drop it, matching the wording of the existing argument-level bootstrap-knob guard.
- [x] 1.3 Confirm the carve-out is additive (runs alongside, not instead of, the unknown-axis and duplicate/`NA`/`nrow` checks) and only fires for the `hc` step under `ci = FALSE`.

## 2. Tests

- [x] 2.1 Add tests asserting `ssd_define_scenario(ci = FALSE, partition_by = list(hc = c(..., "nboot")))` aborts (and the same for `ci_method`, `parametric`).
- [x] 2.2 Add tests asserting `ssd_define_scenario(ci = FALSE, bundle = list(hc = "nboot"))` aborts (and the same for `ci_method`, `parametric`).
- [x] 2.3 Add a test asserting a non-bootstrap hc axis (`est_method`) is still accepted in `partition_by$hc`/`bundle$hc` under `ci = FALSE`.
- [x] 2.4 Add a test asserting bootstrap-only axes are accepted in `partition_by$hc`/`bundle$hc` under `ci = TRUE` (existing behaviour unchanged).
- [x] 2.5 Assert the abort fires in the `ssd_define_scenario()` user-facing frame, not an internal validator frame.
- [x] 2.6 Update the pre-existing "all-axes-in-path yields no inner axes" test to use `ci = TRUE` (its hc path lists every hc axis, including the now-guarded bootstrap axes).

## 3. Documentation & verification

- [x] 3.1 Update the `validate_partition_by()`/`validate_axis_list()` roxygen `@noRd` notes to mention the `ci = FALSE` hc carve-out, and the `partition_by` argument doc on `ssd_define_scenario()`.
- [x] 3.2 Run `air format .`, `devtools::document()`, and `devtools::test()`; confirm green (`FAIL 0 | PASS 455`).
- [ ] 3.3 Run `openspec verify ci-false-partition-guard` (or `/opsx:verify`) and confirm the implementation matches the spec scenarios.
