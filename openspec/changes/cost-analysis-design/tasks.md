## 1. Phase A — rollup seam (buildable now; depends only on landed code)

- [x] 1.1 Create `R/cost-analysis-design.R` and `combine_cost_breakdowns(named_breakdowns)`: row-bind per-member `ci_method` × `nboot` breakdown tibbles into one with a leading `scenario` column (member name), preserving column types; NULL/empty members drop out
- [x] 1.2 `design_cost_totals(named_totals, named_longests)`: observed total = Σ member totals, observed longest = max member longest, both `difftime` (secs); skip members with no value and return the contributing-member count
- [x] 1.3 `pool_calibration_from_frames(named_frames, host = NULL)`: pool the members' measured `(nrow, ci_method, nboot, time, .host)` rows into one sweep; abort listing hosts when >1 distinct `.host` and `host` is `NULL`; otherwise filter to `host`, call the landed `calibrate_coefficients()`/`calibrate_nrow_factor()`/`new_ssdsims_cost_calibration()`, with run-derived-from-design provenance
- [x] 1.4 `design_member_addressing(named_scenarios, root = "results")`: per member derive `prefix = "<name>_"` and the `scenario=<name>` results root (via the landed `scenario_results_dir()`), returning a tibble/list keyed by member name — the addressing `scenario-combine` will mint, computed without `ssd_design()`
- [x] 1.5 `format_design_breakdown(breakdown_with_scenario, total, longest)`: a pure string helper rendering the per-`scenario` breakdown and design totals (reusing `format_duration()`); no S3 method on `ssdsims_cost_analysis` is defined in this phase
- [x] 1.6 `air format .`; confirm Phase A adds no exports, no `man/` churn, and no S3 method on a sibling-owned class

## 2. Phase A — tests (now)

- [x] 2.1 `combine_cost_breakdowns()`: two fixture breakdowns combine into one with the right `scenario` column and row order; an empty/NULL member is dropped
- [x] 2.2 `design_cost_totals()`: totals sum, longest is the max, contributing-member count excludes non-running members
- [x] 2.3 `pool_calibration_from_frames()`: single-host fixture frames pool into a calibration that drives `ssd_estimate_cost()`; a mixed-host set aborts naming the hosts; selecting a host disambiguates
- [x] 2.4 `design_member_addressing()`: real `ssdsims_scenario` objects yield `scenario=<name>` roots and `<name>_` prefixes matching the documented `scenario-combine` shape
- [x] 2.5 `format_design_breakdown()`: snapshot test of the design-aware rendering; assert no `.Random.seed` change and no file writes across the Phase A helpers
- [x] 2.6 Run `devtools::test()` (or `testthat`) and `R CMD check` locally; CI green

## 3. Phase A — wrap-up (now)

- [x] 3.1 Update `ROADMAP.md` with the `cost-analysis-design` entry (in-flight), noting Phase A landed and Phase B gated on `scenario-combine` + `cost-analysis-targets`

## 4. Phase B — dependency gate (blocks the rest)

- [ ] 4.1 Confirm `cost-analysis-targets` has landed on the base branch: `ssd_analyse_cost()`/`ssd_compare_cost()`/`ssd_calibrate_cost_from_run()`, the `ssdsims_cost_analysis`/`ssdsims_cost_comparison` objects, the scenario tar_meta resolver, and the timing columns retained on the compact summary
- [ ] 4.2 Confirm `scenario-combine` has landed on the base branch: `ssd_design()`/`ssdsims_design`, `ssd_design_targets()`'s `<name>_` target-name prefix, the `scenario=<name>/layout=<hash>` roots, and the combined `<root>/summary.parquet` with its `scenario` column

## 5. Phase B — `ssdsims_design` adapter (gated)

- [ ] 5.1 Add `ssd_analyse_cost.ssdsims_design()`: unpack the design into the normalised members, call `ssd_analyse_cost(scenario)` per member under its `scenario=<name>` root, and delegate to `combine_cost_breakdowns()`/`design_cost_totals()`; report the contributing-member count and skip members with no readable run
- [ ] 5.2 Combined-summary fast path: read every member's hc timings from `<root>/summary.parquet` (grouped by its `scenario` column) in one DuckDB read when present, falling back to per-member shard globs with identical totals; the `fit` addend always from per-member `fit` globs
- [ ] 5.3 Design-aware store resolver: one `tar_meta()`; regenerate names per member with the `<name>_` prefix (reusing `scenario-combine`'s helper) and the scenario resolver from `cost-analysis-targets`, join on `name`; per-member envelope overhead and pre-timing fallback summed to the design total; exclude the combined `summary` and `<name>_upload_<step>` targets; report unmatched/`NA` targets, never fatal
- [ ] 5.4 Add `ssd_compare_cost.ssdsims_design()`: design predicted = Σ members' `ssd_estimate_cost()`, beside the design observed analysis; report design totals, ratios, and per-`scenario` rows
- [ ] 5.5 Add `ssd_calibrate_cost_from_run.ssdsims_design()`: read members' measured hc/fit durations and delegate to `pool_calibration_from_frames()` (host-aware)
- [ ] 5.6 Design-aware `format`/`print` methods for `ssdsims_cost_analysis`/`ssdsims_cost_comparison`: call `format_design_breakdown()` when a `scenario` column is present; scenario-level output byte-unchanged

## 6. Phase B — tests, docs (gated)

- [ ] 6.1 End-to-end two-scenario design: `ssd_analyse_cost(design)` totals = sum of per-member `ssd_analyse_cost(scenario)`; per-`scenario` breakdown; longest = max across members; a member with no run excluded with the count reported
- [ ] 6.2 Combined-summary fast path vs per-member-glob fallback give identical totals; store resolver matches prefixed names across `scenario=<name>` roots and excludes `summary`/`upload`; unmatched/`NA` reported not fatal
- [ ] 6.3 `ssd_compare_cost(design)` totals/ratios/per-`scenario` rows; `ssd_calibrate_cost_from_run(design)` pools single-host members (usable by `ssd_estimate_cost()`) and aborts on mixed hosts; design read-only assertions; design-vs-scenario print snapshots
- [ ] 6.4 Cost-analysis vignette design section (analyse → compare → recalibrate over a small two-scenario design); roxygen for the design methods; regenerate `man/`; `_pkgdown.yml` entries; `air format .`
- [ ] 6.5 Update `ROADMAP.md` (Phase B complete) and confirm proposal/spec/design stay in sync with the implemented behaviour
