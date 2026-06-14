## 1. Preflight — dependency gate (blocks all implementation)

- [ ] 1.1 Confirm `cost-analysis-targets` has landed: `ssd_analyse_cost()`, `ssd_compare_cost()`, `ssd_calibrate_cost_from_run()`, the `ssdsims_cost_analysis`/`ssdsims_cost_comparison` objects, the tar_meta target-name resolver, and the timing columns retained on the compact summary all exist on the base branch
- [ ] 1.2 Confirm `scenario-combine` has landed: `ssd_design()`/`ssdsims_design`, `ssd_design_targets()`'s `<name>_` target-name prefix, the `scenario=<name>/layout=<hash>` roots, and the combined `<root>/summary.parquet` with its `scenario` column all exist on the base branch
- [ ] 1.3 Identify and note the reusable internals from both siblings: the prefix/root helpers from `scenario-combine` (target-name prefixing, `scenario=<name>` root construction) and the resolver/fitters from `cost-analysis-targets` (`shard_cell_names()`/`scenario_partition_axes()` name regeneration, `calibrate_coefficients()`/`calibrate_nrow_factor()`)

## 2. `ssd_analyse_cost()` design method

- [ ] 2.1 Add `ssdsims_design` dispatch to `ssd_analyse_cost()` in `R/cost-analysis.R`: loop the design's named members, call the scenario-level machinery per member under its `scenario=<name>` root, and row-bind with a leading `scenario` column on the breakdown
- [ ] 2.2 Compute design totals from the bound members: observed total compute = Σ member totals; observed longest task = max member longest; report the contributing-member count and skip members with no readable run (no abort)
- [ ] 2.3 Implement the combined-summary fast path: when `<root>/summary.parquet` is present and carries the hc timing columns, derive every member's hc observed cost from that single DuckDB read grouped by `scenario` (project id + timing columns only, never decode blobs, never collect)
- [ ] 2.4 Implement the per-member shard-glob fallback for hc when the combined summary is absent or pre-timing, asserting identical totals to the fast path; read the `fit` addend from per-member `fit` globs in both paths

## 3. Design-aware targets store resolver

- [ ] 3.1 Extend the tar_meta resolver to a single store covering all members: regenerate expected target names per member with the design's `<name>_` prefix and the member's `scenario=<name>` root, joining on the store's `name` column (no string parsing), reusing the `scenario-combine` prefix/root helpers
- [ ] 3.2 Compute per-member envelope overhead (target seconds − Σ measured task durations) and the pre-timing proportional fallback (marked inferred), summed to the design total; exclude the combined `summary` and `<name>_upload_<step>` targets from attribution
- [ ] 3.3 Report unmatched targets and `NA`-seconds targets with counts (never silently drop, never abort), and surface the contributing-target count

## 4. `ssd_compare_cost()` and `ssd_calibrate_cost_from_run()` design methods

- [ ] 4.1 Add `ssdsims_design` dispatch to `ssd_compare_cost()`: design predicted = Σ members' `ssd_estimate_cost()`; place beside the design observed analysis; report design-total predicted/observed and ratio for total and longest task, plus a per-`scenario` ratio row
- [ ] 4.2 Add `ssdsims_design` dispatch to `ssd_calibrate_cost_from_run()`: pool the members' measured hc durations into one sweep frame (and pooled measured fit durations for the addend), call the existing fitters, mark provenance run-derived-from-design
- [ ] 4.3 Enforce host-awareness across the design: distinct `.host` values across members are not silently pooled — require an explicit host or abort listing the hosts found; verify the result drops into `ssd_estimate_cost()`

## 5. Design-aware printing

- [ ] 5.1 Branch the `format`/`print` methods of `ssdsims_cost_analysis`/`ssdsims_cost_comparison` on the presence of the `scenario` breakdown column: design output shows the per-`scenario` breakdown above the design totals; scenario-level output is byte-unchanged (reuse `format_duration()` and the existing scaffolding)

## 6. Tests

- [ ] 6.1 Two-scenario design analysis: design totals = sum of per-member `ssd_analyse_cost(scenario)` results; breakdown carries one `scenario` value per member; longest task = max across members
- [ ] 6.2 Combined-summary fast path vs per-member-glob fallback produce identical totals; missing/pre-timing summary routes to the fallback
- [ ] 6.3 Store resolver: prefixed names (`a_hc_step_<cell>`/`b_hc_step_<cell>`) resolve to the right `scenario=<name>` member; combined `summary` and `<name>_upload_<step>` excluded; unmatched/`NA` targets reported not fatal
- [ ] 6.4 `ssd_compare_cost(design)` reports design totals, ratios, and per-`scenario` rows; `ssd_calibrate_cost_from_run(design)` pools single-host members and is usable by `ssd_estimate_cost()`; a mixed-host design aborts listing hosts
- [ ] 6.5 Read-only assertions for the design methods: `.Random.seed` unchanged, no files written, no pipeline run; design-aware vs scenario-level print snapshots
- [ ] 6.6 A member with no readable run is excluded with the contributing-member count reported (keep-going survivors-union)

## 7. Docs

- [ ] 7.1 Add a design section to the cost-analysis vignette: the design-level analyse → compare → recalibrate loop over a small two-scenario design, framing the design observed total as serial-equivalent compute summed across members
- [ ] 7.2 Roxygen for the design methods; regenerate `man/`; add/adjust `_pkgdown.yml` reference entries; run `air format .`
- [ ] 7.3 Update `ROADMAP.md` (the cost-analysis-design entry) and confirm the proposal/spec/design stay in sync with the implemented behaviour
