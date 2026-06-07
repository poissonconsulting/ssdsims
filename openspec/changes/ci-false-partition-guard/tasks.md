## 1. Validation guard

- [ ] 1.1 Add a `ci` parameter to `validate_partition_by()` (`R/scenario.R`) and thread it through to `validate_axis_list()`; pass `ci` at the `validate_partition_by()` call site (`R/scenario.R:284`).
- [ ] 1.2 In `validate_axis_list()`, after the existing generic `task_axes(step)` subset check, add an `isFALSE(ci)` branch that, for `step == "hc"`, rejects any axis in `c("nboot", "ci_method", "parametric")` — aborting via `chk::abort_chk(..., call = call)` (plain loop, no `purrr::walk`), naming the offending axis/axes and directing the user to set `ci = TRUE` or drop it, matching the wording of the existing argument-level bootstrap-knob guard.
- [ ] 1.3 Confirm the carve-out is additive (runs alongside, not instead of, the unknown-axis and duplicate/`NA`/`nrow` checks) and only fires for the `hc` step under `ci = FALSE`.

## 2. Tests

- [ ] 2.1 Add tests asserting `ssd_define_scenario(ci = FALSE, partition_by = list(hc = c(..., "nboot")))` aborts (and the same for `ci_method`, `parametric`).
- [ ] 2.2 Add tests asserting `ssd_define_scenario(ci = FALSE, bundle = list(hc = "nboot"))` aborts (and the same for `ci_method`, `parametric`).
- [ ] 2.3 Add a test asserting a non-bootstrap hc axis (`est_method`) is still accepted in `partition_by$hc`/`bundle$hc` under `ci = FALSE`.
- [ ] 2.4 Add a test asserting bootstrap-only axes are accepted in `partition_by$hc`/`bundle$hc` under `ci = TRUE` (existing behaviour unchanged).
- [ ] 2.5 Assert the abort fires in the `ssd_define_scenario()` user-facing frame, not an internal validator frame.

## 3. Documentation & verification

- [ ] 3.1 Update the `validate_partition_by()`/`validate_axis_list()` roxygen `@noRd` notes to mention the `ci = FALSE` hc carve-out; update the `partition_by`/`ci` argument docs on `ssd_define_scenario()` if they enumerate the validation rules.
- [ ] 3.2 Run `air format .`, then `devtools::document()` and `devtools::test()` (at least the scenario-definition tests); confirm green.
- [ ] 3.3 Run `openspec verify ci-false-partition-guard` (or `/opsx:verify`) and confirm the implementation matches the spec scenarios.
