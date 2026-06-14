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
- [x] 1.5 Document the invalidation consequence in `ssd_scenario_targets()`'s
  roxygen (fit/hc file hashes are volatile across recomputes; the §8.3 pin
  covers code-edit recomputes).

## 2. Target-name → shard resolver (tar_meta layer)

- [x] 2.1 Add an internal resolver in `R/cost-analysis.R` that regenerates the
  expected `<step>_step_<pathcell>` names from the scenario (reusing
  `shard_cell_names()`/`scenario_partition_axes()` logic) and joins them to
  `tar_meta()`'s `name`/`seconds`; exclude `summary`/`upload_<step>`; report
  matched/unmatched counts; exclude `NA`-seconds targets from totals.
- [x] 2.2 Unit-test the resolver against `ssd_scenario_<step>_shards()` for a
  small scenario (names match; non-shard names excluded; unmatched reported).

## 3. `ssd_analyse_cost()`

- [x] 3.1 Implement `ssd_analyse_cost()`: read the fit/hc shard glob projecting
  only id + timing columns at the DuckDB level (never decoding blobs), compute
  measured per-task durations, and return an `ssdsims_cost_analysis` (observed
  total, observed longest task, `ci_method` × `nboot` breakdown keyed like
  `ssd_estimate_cost()`'s); accept a baseline result as input too.
- [x] 3.2 When a `targets` store is supplied, add the per-shard envelope
  (`target seconds − Σ task durations`) via the task-2 resolver; for pre-timing
  shards fall back to proportional-to-prediction attribution and mark the
  result inferred rather than measured.
- [x] 3.3 Add `format`/`print` methods reusing `format_duration()`; show
  measured vs inferred provenance and (when available) longest task vs longest
  shard envelope.

## 4. `ssd_calibrate_cost_from_run()`

- [x] 4.1 Build the sweep frame (`nrow`, `ci_method`, `nboot`, `time`) from
  measured hc task durations and derive the fixed addend from measured fit
  durations; fit via the existing `calibrate_coefficients()` /
  `calibrate_nrow_factor()`.
- [x] 4.2 Make it host-aware: never silently pool distinct `.host` values —
  explicit host argument or abort listing the hosts found.
- [x] 4.3 Set run-derived provenance (source, observed-run date); test the
  result drops into `ssd_estimate_cost()` unchanged.

## 5. `ssd_compare_cost()`

- [x] 5.1 Implement `ssd_compare_cost()` placing `ssd_estimate_cost()` beside
  `ssd_analyse_cost()`; return an `ssdsims_cost_comparison` with
  predicted/observed totals, longest, and their ratios.
- [x] 5.2 Add `format`/`print` methods.

## 6. Read-only guarantees & tests

- [x] 6.1 Test that `ssd_analyse_cost()` / `ssd_compare_cost()` leave
  `.Random.seed` unchanged and write no files.
- [x] 6.2 Build a deterministic fixture: a tiny scenario run through
  `ssd_run_scenario_shards()` (timing columns in-band) plus a synthesised
  `tar_meta()`-shaped tibble for the envelope/fallback paths — no live cluster
  needed.
- [x] 6.3 Snapshot the `print()` output of the new objects (use the
  deterministic `test_cost_calibration()` fixture pattern).

## 7. Exports, docs, vignette

- [x] 7.1 Roxygen-document the new functions; `devtools::document()` for
  `NAMESPACE` exports and S3 methods.
- [x] 7.2 Add the new functions to `_pkgdown.yml`'s cost reference section.
- [x] 7.3 Add the `cost-analysis` vignette demonstrating analyse → compare →
  recalibrate on a small worked run; frame the observed total as
  serial-equivalent compute, distinct from wall time under workers.
- [x] 7.4 Any proof-of-work scripts go under
  `openspec/changes/cost-analysis-targets/exploration/` (never the repo top
  level).

## 8. Finalise

- [x] 8.1 Format with `air`; `devtools::check()` clean.
- [x] 8.2 `openspec validate cost-analysis-targets --strict` passes.
- [x] 8.3 Update `ROADMAP.md` (in-flight marker) and `TARGETS-DESIGN.md` /
  `GLOSSARY.md` if the timing columns warrant a mention in the shard contract
  prose.

## 9. Design-level rollup (depends on `scenario-combine`; gated)

These tasks require `scenario-combine`'s `ssd_design()` / `ssdsims_design`,
`ssd_design_targets()`'s **shared** `<root>/seed=<value>/layout=<hash>` trees with
seed-woven shard target names (`<step>_step_<seed>_<pathcell>`), the landed
`design_results_dir()`, and the combined `<root>/summary.parquet` with its
`scenario` column. (The originally-drafted `scenario=<name>` roots / `<name>_`
prefix were **not** shipped by `scenario-combine`; per-member cost is recovered by
filtering shared shards on the member's `hc_id`/`fit_id`s, as
`ssd_summarise_member()` does.) They build on the scenario-level functions from
groups 2–5 above. A working prototype of the collection-agnostic aggregation
seam these methods delegate to lives, **as proof of work only**, under
`exploration/design-rollup-seam/` — promote `combine_cost_breakdowns()`,
`design_cost_totals()`, `pool_calibration_from_frames()`, and
`format_design_breakdown()` into `R/` rather than rewriting them; its
`design_member_addressing()` is **superseded** by `design_results_dir()` +
`hc_id`/`fit_id` filtering and is not promoted.

- [ ] 9.1 Add `ssd_analyse_cost.ssdsims_design()`: unpack the design into its
  named members, analyse each member from its seed group's shared
  `design_results_dir(member, root)` tree **filtered to the member's
  `hc_id`/`fit_id`s** (the scenario-level core, applied to the member's filtered
  timings), and aggregate into an `ssdsims_cost_analysis` whose breakdown carries
  a leading `scenario` column and design totals (observed total = Σ members,
  per-member accounting; longest = max members); skip members with no readable run
  and report the contributing-member count.
- [ ] 9.2 Combined-summary fast path: read every member's hc timings from
  `<root>/summary.parquet` (grouped by its `scenario` column; the per-member
  summaries retain the timing columns) in one DuckDB read when present, falling
  back to per-member reads of the seed-group `hc` tree filtered by `hc_id` with
  identical totals; the `fit` addend always from the member's seed-group `fit`
  shards filtered by `fit_id`.
- [ ] 9.3 Design-aware store resolver: one `tar_meta()`; regenerate the seed-woven
  shard names **per seed group** (`<step>_step_<seed>_<pathcell>` from the union
  shard tables keyed on `c("seed", <path axes>)`, reusing the group-2 resolver
  logic), join on `name`; the per-shard envelope overhead computed **once per
  shared shard** (per seed group) and summed to the design total; exclude the
  combined `summary`, every per-member `summary_<name>`, and every `upload_<step>`
  target; report unmatched/`NA` targets, never fatal.
- [ ] 9.4 Add `ssd_compare_cost.ssdsims_design()` (design predicted = Σ members'
  `ssd_estimate_cost()`, beside the design observed analysis; design totals,
  ratios, and per-`scenario` rows) and
  `ssd_calibrate_cost_from_run.ssdsims_design()` (pool the members' measured
  hc/fit durations into one host-aware recalibration via
  `pool_calibration_from_frames()`).
- [ ] 9.5 Design-aware `format`/`print` for `ssdsims_cost_analysis` /
  `ssdsims_cost_comparison`: render the per-`scenario` breakdown and design totals
  when a `scenario` column is present; scenario-level output byte-unchanged.
- [ ] 9.6 Tests: end-to-end two-scenario design (design totals = sum of per-member
  analyses; per-`scenario` breakdown; a member with no run excluded with the count
  reported); fast-path vs per-member-read fallback identical totals; seed-woven
  store resolver across the shared `seed=/layout=` trees; mixed-host design aborts;
  design-vs-scenario print snapshots; read-only assertions.
- [ ] 9.7 Extend the `cost-analysis` vignette with a design section (analyse →
  compare → recalibrate over a small two-scenario design); roxygen + `man/` for
  the design methods; `_pkgdown.yml` entries.
