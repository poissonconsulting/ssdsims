## 1. Exploration / correctness oracle

- [x] 1.1 Write `exploration/distset-subset-invariance.R`: at a fixed seed, show `ssdtools::ssd_hc(subset(union_fit, set, strict = FALSE))` is byte-identical to `ssd_hc(ssd_fit_dists(data, dists = set))` for both averaged pools (BCANZ, Iwasaki) and single-distribution sets, with `ci = FALSE` and `ci = TRUE`. This is the premise of the whole change; capture it before coding.

## 2. `ssd_distset()` constructor and scenario input

- [x] 2.1 Add `R/distset.R`: `ssd_distset(...)` returning an `ssdsims_distset` collection (named list of distribution-name character vectors). Names from `...` names; validate unique/non-missing/filesystem-safe. Validate each set: non-empty, unique, non-`NA` character ⊆ `ssdtools::ssd_dists_all()`. Abort in the constructor's context naming the offending set. Add a snapshot-stable `print.ssdsims_distset()`; export and `devtools::document()`.
- [x] 2.2 In `R/scenario.R`, change `dists` to accept **only** an `ssd_distset()` collection; a bare character vector or plain list SHALL abort with a message naming `ssd_distset()` (no expression-archaeology for set names).
- [x] 2.3 Derive the fit union `sort(unique(unlist(dists)))` and store it as `scenario$fit$dists`; store the named sets as `scenario$hc$distsets` (member vectors keyed by set name).
- [x] 2.4 Keep `dists` in the simulation-settings block of the signature (it remains a fit-level setting feeding the union); update its `@param`/roxygen to describe the `ssd_distset()` form and the union/by-name semantics.
- [x] 2.5 Render the distribution sets in `print.ssdsims_scenario()` via `fmt_grid_value()` (set names + members, no large dumps).

## 3. Axis vocabulary and hc task table

- [x] 3.1 Add `"distset"` to `task_axes("hc")` in `R/task-lists.R` (hc axes become `nboot`, `ci_method`, `parametric`, `distset`).
- [x] 3.2 Derive the hc task table by crossing each fit-task identity with the hc grid **and** the `distset` names: `ci = FALSE` → `D` rows per fit task (one per set, bootstrap scenario options `NA`); `ci = TRUE` → `distset × nboot × ci_method × parametric`. Each hc row carries its `distset` name and parent `fit_id`. A single-set collection (`ssd_distset(BCANZ = ...)`) yields one `distset` value (one hc row per fit task when `ci = FALSE`).
- [x] 3.3 Confirm `partition_by`/`bundle` validation accepts `"distset"` for the `hc` step (falls out of `task_axes("hc")`); keep the default hc path `c("dataset", "sim")` so `distset` is inner by default.

## 4. Accessor and scenario slice

- [x] 4.1 Add `scenario_distset(scenario, name)` in `R/accessors.R` returning the set's member vector from `scenario$hc$distsets`, aborting on an unknown name; export it (`@export`) and `devtools::document()`.
- [x] 4.2 Extend the `hc` branch of `scenario_step_slice()` (`R/targets-runner.R`) to carry `hc$distsets` (so the hc runner can resolve `distset` → members), keeping the slice a pure/deterministic function and the `ssdsims_scenario` class.

## 5. hc execution: subset the union fit per distset

- [x] 5.1 In `hc_data_task_primer()` (`R/task-lists.R:605`), accept the set members and `subset(fits, select = members, strict = FALSE)` before `ssd_hc()`; an empty subset returns zero rows (no abort). This single chokepoint keeps all three runners byte-identical.
- [x] 5.2 In `ssd_run_hc_step()` (`R/targets-runner.R`) — and the single-core `R/shard-runner.R` path — decode each parent union fit **once** per `fit_id`, resolve each hc task's `distset` to members via `scenario_distset()`, subset, and tag each output row with `hc_id`, `fit_id`, and `distset`. Reuse the one decoded fit across all `distset` tasks sharing the `fit_id`.
- [x] 5.3 Keep the in-memory `ssd_hc_sims()` path (`R/hc-sims.R`/`R/internal.R`) consistent with the subset-then-average contract (and the baseline runner `ssd_run_scenario_baseline()`), so baseline == shards == targets.
- [x] 5.4 Ensure the hc Parquet/summary carries an explicit `distset` column (it disambiguates rows within a bundled shard) consistent with the `distset=` path form.

## 6. Tests

- [x] 6.1 Same-seed subset-reuse invariant unit test (per §1.1): collapsed pool result equals direct-fit-then-hc at the same primer, across averaged and single-dist sets and both `ci` values. NOT an old-vs-new pipeline equality.
- [x] 6.2 Byte-identity test: baseline runner, single-core shard runner, and `targets` pipeline agree per task for a multi-set scenario.
- [x] 6.3 Path-axis-growth test: adding a distribution set (with `distset` on the hc path) mints only new hc shards and caches all sample/fit shards and pre-existing hc shards.
- [x] 6.4 Task-table tests: `D` sets multiply hc rows by `D` (not by `est_method`); a single-set `ssd_distset()` yields one hc row per fit task; a bare-vector / plain-list `dists` aborts with a message naming `ssd_distset()`.
- [x] 6.5 Empty-subset test: a set whose members all dropped yields zero hc rows, and `ssd_summarise()` unions the survivors.

## 7. Docs, snapshots, decision log

- [x] 7.1 Update `TARGETS-DESIGN.md`: the `dists-simulation-setting` decision log entry and the §"No nested reuse" note get an addendum — *individual distributions still never fan out*; a named set of pools is an hc-level axis over post-fit subsets of one union fit (reuse now holds within one union).
- [x] 7.2 Update `GLOSSARY.md` (define "distribution set" / `distset` axis) and `task_axes("hc")` references that list the hc axes.
- [x] 7.3 Update the `defining-a-scenario` and `sharded-pipeline` vignettes with a multi-set example (and the bundled-vs-path `distset` trade-off); add a worked iwasaki-style snippet.
- [x] 7.4 Re-record affected snapshots (printed scenarios; hc task-count assertions; `ci = TRUE` CI snapshots re-seeded by the primer change).
- [x] 7.5 Run `air format .`, `devtools::document()`, `devtools::test()`, `devtools::check()`; verify a multi-set `ci = FALSE` scenario fits the union once and re-averages per pool (one fit, cheap per-pool hc).
- [x] 7.6 Move the ROADMAP item for distribution sets to Done with the archive link on completion.
