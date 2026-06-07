## 1. Signature and storage

- [ ] 1.1 In `R/scenario.R`, move the `dists` formal out of the fit-axis block of `ssd_define_scenario()` to lead the simulation-settings block: `… parametric, dists, proportion, ci, samples, partition_by, bundle` (the `cloud-upload` change removed the trailing `upload` formal)
- [ ] 1.2 Confirm storage is unchanged — `dists` still validated as a unique character vector and stored at `scenario$fit$dists`; no change to `scenario$hc`
- [ ] 1.3 Confirm `task_axes()` is untouched (it already excludes `dists`); no change to `R/task-lists.R`, primers, identities, or shard layout

## 2. Print and docs

- [ ] 2.1 Update `print.ssdsims_scenario()` so `dists` renders among the **fit** knobs marked as a setting (not an axis), the hc knobs keep role order (axes then `proportion`/`ci`/`samples`)
- [ ] 2.2 Update the `ssd_define_scenario()` roxygen: document `dists` as a fit-level simulation setting; regenerate `man/`

## 3. Call-site sweep (`canonical-call-sites`)

- [ ] 3.1 Reorder `dists` to its new signature position in every `ssd_define_scenario()` call: `@examples`, `tests/`, `tests/testthat/_snaps/`, `scripts/`, `vignettes/`, and `inst/targets-templates/`
- [ ] 3.2 Re-record any scenario `print()` snapshots affected by the fit/hc knob grouping

## 4. Tests

- [ ] 4.1 Assert `task_axes("fit")` excludes `"dists"` and that two scenarios differing only in `dists` produce identical fit/hc `*_id` keys and primers (behaviour-preserving: `dists` is not part of identity)
- [ ] 4.2 Assert the signature order: `dists`, `proportion`, `ci`, `samples` are contiguous and follow `parametric`
- [ ] 4.3 `air format` and `R CMD check` clean; `devtools::test()` green

## 5. Sync

- [ ] 5.1 After implementation, sync the `scenario-definition` delta into `openspec/specs/scenario-definition/spec.md` and resolve the GLOSSARY.md note ("moving `dists` … lands via the `dists-simulation-setting` change")
