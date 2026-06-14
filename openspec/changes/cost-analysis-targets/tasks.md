## 1. Timing instrumentation (fit/hc runners + baseline)

- [x] 1.1 Bracket each task body in `ssd_run_fit_step()` and `ssd_run_hc_step()`
  with `Sys.time()` and write `.start`/`.end` (UTC, Parquet TIMESTAMP) and
  `.host` (`cost_cpu_info()`) columns into the shard rows; on hc the values
  repeat across a task's `proportion` rows. `sample` runner untouched.
- [x] 1.2 Add the same columns to `ssd_run_scenario_baseline()`'s in-memory
  `fit`/`hc` tibbles (legacy `ssd_*_sims` untouched).
- [x] 1.3 Keep `.start`/`.end`/`.host` in both `ssd_summarise()` outputs while
  still projecting out `dists`/`samples` from the compact summary.
- [x] 1.4 Migrate the byte-identity oracle, re-layout, and atomic-rewrite tests
  to result-column comparisons (timing columns excluded, joined on `<step>_id`);
  add a test that `sample` shards stay file-level byte-identical and that
  fit/hc result columns are unchanged by instrumentation; update shard-schema
  snapshots.
- [ ] 1.5 Document the invalidation consequence in `ssd_scenario_targets()`'s
  roxygen (fit/hc file hashes are volatile across recomputes; the §8.3 pin
  covers code-edit recomputes).

## 2. Target-name → shard resolver (tar_meta layer)

- [ ] 2.1 Add an internal resolver in `R/cost-analysis.R` that regenerates the
  expected `<step>_step_<pathcell>` names from the scenario (reusing
  `shard_cell_names()`/`scenario_partition_axes()` logic) and joins them to
  `tar_meta()`'s `name`/`seconds`; exclude `summary`/`upload_<step>`; report
  matched/unmatched counts; exclude `NA`-seconds targets from totals.
- [ ] 2.2 Unit-test the resolver against `ssd_scenario_<step>_shards()` for a
  small scenario (names match; non-shard names excluded; unmatched reported).

## 3. `ssd_analyse_cost()`

- [ ] 3.1 Implement `ssd_analyse_cost()`: read the fit/hc shard glob projecting
  only id + timing columns at the DuckDB level (never decoding blobs), compute
  measured per-task durations, and return an `ssdsims_cost_analysis` (observed
  total, observed longest task, `ci_method` × `nboot` breakdown keyed like
  `ssd_estimate_cost()`'s); accept a baseline result as input too.
- [ ] 3.2 When a `targets` store is supplied, add the per-shard envelope
  (`target seconds − Σ task durations`) via the task-2 resolver; for pre-timing
  shards fall back to proportional-to-prediction attribution and mark the
  result inferred rather than measured.
- [ ] 3.3 Add `format`/`print` methods reusing `format_duration()`; show
  measured vs inferred provenance and (when available) longest task vs longest
  shard envelope.

## 4. `ssd_calibrate_cost_from_run()`

- [ ] 4.1 Build the sweep frame (`nrow`, `ci_method`, `nboot`, `time`) from
  measured hc task durations and derive the fixed addend from measured fit
  durations; fit via the existing `calibrate_coefficients()` /
  `calibrate_nrow_factor()`.
- [ ] 4.2 Make it host-aware: never silently pool distinct `.host` values —
  explicit host argument or abort listing the hosts found.
- [ ] 4.3 Set run-derived provenance (source, observed-run date); test the
  result drops into `ssd_estimate_cost()` unchanged.

## 5. `ssd_compare_cost()`

- [ ] 5.1 Implement `ssd_compare_cost()` placing `ssd_estimate_cost()` beside
  `ssd_analyse_cost()`; return an `ssdsims_cost_comparison` with
  predicted/observed totals, longest, and their ratios.
- [ ] 5.2 Add `format`/`print` methods.

## 6. Read-only guarantees & tests

- [ ] 6.1 Test that `ssd_analyse_cost()` / `ssd_compare_cost()` leave
  `.Random.seed` unchanged and write no files.
- [ ] 6.2 Build a deterministic fixture: a tiny scenario run through
  `ssd_run_scenario_shards()` (timing columns in-band) plus a synthesised
  `tar_meta()`-shaped tibble for the envelope/fallback paths — no live cluster
  needed.
- [ ] 6.3 Snapshot the `print()` output of the new objects (use the
  deterministic `test_cost_calibration()` fixture pattern).

## 7. Exports, docs, vignette

- [ ] 7.1 Roxygen-document the new functions; `devtools::document()` for
  `NAMESPACE` exports and S3 methods.
- [ ] 7.2 Add the new functions to `_pkgdown.yml`'s cost reference section.
- [ ] 7.3 Add the `cost-analysis` vignette demonstrating analyse → compare →
  recalibrate on a small worked run; frame the observed total as
  serial-equivalent compute, distinct from wall time under workers.
- [ ] 7.4 Any proof-of-work scripts go under
  `openspec/changes/cost-analysis-targets/exploration/` (never the repo top
  level).

## 8. Finalise

- [ ] 8.1 Format with `air`; `devtools::check()` clean.
- [ ] 8.2 `openspec validate cost-analysis-targets --strict` passes.
- [ ] 8.3 Update `ROADMAP.md` (in-flight marker) and `TARGETS-DESIGN.md` /
  `GLOSSARY.md` if the timing columns warrant a mention in the shard contract
  prose.

## 9. Design-level rollup (depends on `scenario-combine`; gated)

These tasks require `scenario-combine`'s `ssd_design()` / `ssdsims_design`,
`ssd_design_targets()`'s `<name>_` target-name prefix, the
`scenario=<name>/layout=<hash>` roots, and the combined `<root>/summary.parquet`
with its `scenario` column. They build on the scenario-level functions from
groups 2–5 above. A working prototype of the collection-agnostic aggregation
seam these methods delegate to lives, **as proof of work only**, under
`exploration/design-rollup-seam/` — promote it into `R/` here rather than
rewriting it.

- [ ] 9.1 Add `ssd_analyse_cost.ssdsims_design()`: unpack the design into its
  named `(scenario, results-root)` members, call the scenario-level
  `ssd_analyse_cost()` per member under its `scenario=<name>` root, and aggregate
  into an `ssdsims_cost_analysis` whose breakdown carries a leading `scenario`
  column and design totals (observed total = Σ members, longest = max members);
  skip members with no readable run and report the contributing-member count.
- [ ] 9.2 Combined-summary fast path: read every member's hc timings from
  `<root>/summary.parquet` (grouped by its `scenario` column) in one DuckDB read
  when present, falling back to the per-member shard globs with identical totals;
  the `fit` addend always from the per-member `fit` globs.
- [ ] 9.3 Design-aware store resolver: one `tar_meta()`; regenerate names per
  member with the `<name>_` prefix (reusing `scenario-combine`'s helper) and the
  group-2 scenario resolver, join on `name`; per-member envelope overhead and the
  pre-timing fallback summed to the design total; exclude the combined `summary`
  and `<name>_upload_<step>` targets; report unmatched/`NA` targets, never fatal.
- [ ] 9.4 Add `ssd_compare_cost.ssdsims_design()` (design predicted = Σ members'
  `ssd_estimate_cost()`, beside the design observed analysis; design totals,
  ratios, and per-`scenario` rows) and
  `ssd_calibrate_cost_from_run.ssdsims_design()` (pool the members' measured
  hc/fit durations into one host-aware recalibration).
- [ ] 9.5 Design-aware `format`/`print` for `ssdsims_cost_analysis` /
  `ssdsims_cost_comparison`: render the per-`scenario` breakdown and design totals
  when a `scenario` column is present; scenario-level output byte-unchanged.
- [ ] 9.6 Tests: end-to-end two-scenario design (design totals = sum of per-member
  analyses; per-`scenario` breakdown; a member with no run excluded with the count
  reported); fast-path vs per-member-glob fallback identical totals; prefixed-name
  store resolver across `scenario=<name>` roots; mixed-host design aborts;
  design-vs-scenario print snapshots; read-only assertions.
- [ ] 9.7 Extend the `cost-analysis` vignette with a design section (analyse →
  compare → recalibrate over a small two-scenario design); roxygen + `man/` for
  the design methods; `_pkgdown.yml` entries.
