## 1. Target-name → shard resolver

- [ ] 1.1 Add an internal resolver in `R/cost-analysis.R` that regenerates the
  expected `<step>_step_<pathcell>` shard target names for a scenario (reusing
  `scenario_partition_axes()` / the `shard_cell_names()` naming logic) and
  returns a tibble mapping each shard target name to its step and the shard's
  `tasks` rows.
- [ ] 1.2 Join the store's `tar_meta()` `name`/`seconds` rows to the regenerated
  names; exclude non-shard targets (`summary`, `upload_<step>`) from per-task
  attribution; report the count of matched shard targets and surface unmatched
  ones rather than aborting.
- [ ] 1.3 Unit-test the resolver against `ssd_scenario_<step>_shards()` output for
  a small scenario (names match the shards; non-shard names excluded).

## 2. `ssd_analyse_cost()`

- [ ] 2.1 Implement `ssd_analyse_cost(scenario, store = targets::tar_config_get("store"))`:
  `rlang::check_installed("targets")`, read the store, attribute shard `seconds`,
  return an `ssdsims_cost_analysis` object (observed total, observed longest
  shard, per-axis `ci_method` × `nboot` breakdown).
- [ ] 2.2 Attribute a multi-task `hc` shard's seconds to its tasks proportional to
  the calibrated predicted per-task seconds (design "Decisions"); aggregate the
  breakdown keyed identically to `ssd_estimate_cost()`.
- [ ] 2.3 Exclude `NA`/missing-`seconds` (errored/unbuilt) targets from totals and
  report the contributing-target count.
- [ ] 2.4 Add `format.ssdsims_cost_analysis` / `print.ssdsims_cost_analysis`
  reusing `format_duration()`, mirroring the `ssdsims_cost_estimate` methods.

## 3. `ssd_calibrate_cost_from_run()`

- [ ] 3.1 Implement `ssd_calibrate_cost_from_run(scenario, store = ...)`: build the
  `sweep` frame (`nrow`, `ci_method`, `nboot`, `time`) from observed `hc`-shard
  per-task seconds and call the existing `calibrate_coefficients()` /
  `calibrate_nrow_factor()` to return an `ssdsims_cost_calibration`.
- [ ] 3.2 Set provenance to mark the calibration run-derived (a `source = "run"`
  marker, the store path, the observed-run date) so it is distinguishable from a
  `ssd_calibrate_cost()` sweep result; confirm it drops into `ssd_estimate_cost()`.

## 4. `ssd_compare_cost()`

- [ ] 4.1 Implement `ssd_compare_cost(scenario, store = ..., calibration = ssd_cost_calibration())`
  placing `ssd_estimate_cost()` beside `ssd_analyse_cost()`; return an
  `ssdsims_cost_comparison` reporting predicted/observed total and longest plus
  the predicted/observed ratios.
- [ ] 4.2 Add `format.ssdsims_cost_comparison` / `print.ssdsims_cost_comparison`.

## 5. Read-only guarantees & tests

- [ ] 5.1 Test that `ssd_analyse_cost()` / `ssd_compare_cost()` leave
  `.Random.seed` unchanged and write no files (a `withr::with_tempdir` + seed
  snapshot assertion, mirroring the cost-estimation read-only tests).
- [ ] 5.2 Add a deterministic store fixture (a tiny scenario run, or a synthesised
  `tar_meta()`-shaped tibble) under `tests/testthat/` so the analysis assertions
  and snapshots do not require a live cluster run.
- [ ] 5.3 Snapshot the `print()` output of the three new objects.

## 6. Exports, docs, vignette

- [ ] 6.1 Roxygen-document the new functions; run `devtools::document()` so
  `NAMESPACE` gains the exports and S3 methods.
- [ ] 6.2 Add the new functions to `_pkgdown.yml`'s cost reference section.
- [ ] 6.3 Add a `cost-analysis` vignette demonstrating the analyse → compare →
  recalibrate loop on a small worked run; frame "total" as serial-equivalent.
- [ ] 6.4 If the model's *form* discovery needs proof-of-work, put scripts under
  `openspec/changes/cost-analysis-targets/exploration/` (never the repo top level).

## 7. Finalise

- [ ] 7.1 Format with `air`; run `devtools::check()` (or `R CMD check`) clean.
- [ ] 7.2 `openspec validate cost-analysis-targets --strict` passes.
- [ ] 7.3 Update `ROADMAP.md`: move `[cost-analysis-targets]` to its in-flight
  state and add a `NEWS.md` bullet if appropriate.
