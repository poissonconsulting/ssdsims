## 1. Timing instrumentation (fit/hc runners + baseline)

- [ ] 1.1 Bracket each task body in `ssd_run_fit_step()` and `ssd_run_hc_step()`
  with `Sys.time()` and write `.start`/`.end` (UTC, Parquet TIMESTAMP) and
  `.host` (`cost_cpu_info()`) columns into the shard rows; on `hc` the values
  repeat across a task's `proportion` rows. `sample` runner untouched.
- [ ] 1.2 Add the same columns to `ssd_run_scenario_baseline()`'s in-memory
  `fit`/`hc` tibbles (legacy `ssd_*_sims` untouched).
- [ ] 1.3 Keep `.start`/`.end`/`.host` in both `ssd_summarise()` outputs while
  still projecting out `dists`/`samples` from the compact summary.
- [ ] 1.4 Migrate the byte-identity oracle, re-layout, and atomic-rewrite tests
  to result-column comparisons (timing columns excluded, joined on `<step>_id`);
  add a test that `sample` shards stay file-level byte-identical and that
  `fit`/`hc` result columns are unchanged by instrumentation; update shard-schema
  snapshots.
- [ ] 1.5 Document the invalidation consequence in `ssd_scenario_targets()`'s
  roxygen (fit/hc file hashes are volatile across recomputes; the §8.3 pin
  covers code-edit recomputes).

## 2. `ssd_calibrate_cost_from_run()`

- [ ] 2.1 Implement `ssd_calibrate_cost_from_run(summary, ...)` in
  `R/cost-estimate.R`: accept the `ssd_summarise()` summary (tibble or Parquet
  path), derive per-task `hc` durations (`.end − .start`), build the sweep frame
  (`nrow`, `ci_method`, `nboot`, `time`), and fit via the existing
  `calibrate_coefficients()` / `calibrate_nrow_factor()`; derive `fixed_addend`
  from measured `fit` durations where present.
- [ ] 2.2 Read provenance from the run (`.host` CPU, observed-run date) and mark
  the calibration run-derived; reuse `new_ssdsims_cost_calibration()`.
- [ ] 2.3 Host-awareness: never silently pool distinct `.host` values — explicit
  host argument or abort listing the hosts found.
- [ ] 2.4 Tests: result drops into `ssd_estimate_cost()` unchanged; read-only
  (`.Random.seed` unchanged, no files written, no `targets` required); mixed-host
  summary aborts; provenance marks run-derived. Reuse the deterministic
  `test_cost_calibration()` fixture pattern; build a tiny run summary via
  `ssd_run_scenario_shards()` for the fixture.

## 3. Docs

- [ ] 3.1 Roxygen-document `ssd_calibrate_cost_from_run()`; `devtools::document()`
  for the `NAMESPACE` export.
- [ ] 3.2 Add it to `_pkgdown.yml`'s cost reference section.
- [ ] 3.3 Extend the cost-estimation vignette (or a sibling) with the
  summary → calibrate → estimate → compare loop: read a run summary, total/rank
  observed durations with `dplyr` (`.end − .start`), calibrate, re-estimate, and
  join predicted beside observed — framing the observed total as
  serial-equivalent compute, distinct from wall time under workers. No
  `analyse`/`compare` functions.
- [ ] 3.4 Any proof-of-work scripts go under
  `openspec/changes/cost-analysis-targets/exploration/` (never the repo top level).

## 4. Finalise

- [ ] 4.1 Format with `air`; `devtools::check()` clean.
- [ ] 4.2 `openspec validate cost-analysis-targets --strict` passes.
- [ ] 4.3 Update `ROADMAP.md` (in-flight marker); note the timing columns in
  `TARGETS-DESIGN.md` / `GLOSSARY.md` shard-contract prose if warranted.
