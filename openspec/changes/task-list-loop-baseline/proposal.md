## Why

The targets redesign (`TARGETS-DESIGN.md` §1–§2) expands a declarative `ssdsims_scenario` into a flat set of per-task rows — one row per cross-join cell of each pipeline step — that later steps shard, seed, and route through `targets`. `ssd-define-scenario` (the DAG root, now landed) gives us the declarative object but nothing consumes it yet. This step turns the scenario into the **task-table data shape** the rest of the roadmap is built on, plus a trivial runner, so every subsequent step (`task-primer`, `state-primitives`, `task-tables`, shards, Hive partitioning) can swap in one piece at a time against a known-good baseline. It is the roadmap entry immediately after `ssd-define-scenario` (`define → baseline` in the §12 DAG) and is now unblocked.

## What Changes

- Add task-list derivation that expands an `ssdsims_scenario` into **four task tables** (tibbles):
  - `sample` tasks — one row per `(dataset, sim, replace)` cross-join cell: the single random draw of `n_max = max(nrow)` rows that every `nrow` value sub-truncates (§5). `nrow` is **not** a sample axis; the draw size is carried as the ordinary `n_max` column.
  - `data` tasks — each sample-task identity crossed with the scenario's `nrow` values; each `data` task is the `head(sample, nrow)` truncation. Because truncation is RNG-free, `nrow` is a genuine cross-join axis here without duplicating the shared draw.
  - `fit` tasks — each data-task identity crossed with the `fit` arg grid (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`).
  - `hc` tasks — each fit-task identity crossed with the `hc` arg grid (`nboot`, `est_method`, `ci_method`, `parametric`), respecting the construction-time `ci = FALSE` collapse already recorded on the scenario (§1.2).
- One column per cross-join axis; the tables are the canonical record of "what work exists". Splitting the draw (`sample`) from the truncation (`data`) keeps the §5 sub-truncation property *structural* — one draw shared across every `nrow` — while letting `nrow` be a clean axis downstream.
- **Explicit dependencies.** Each row carries a path-style `<step>_id` primary key (the Hive partition path, §5/§6) plus its parent step's id as a foreign key (`sample_id` on data, `data_id` on fit, `fit_id` on hc), so a child references its parent by a single joinable column.
- Each task table is an `ssdsims_tasks` S3 object (a classed tibble carrying the step name as an attribute) with a `print.ssdsims_tasks()` method that renders nicely: the step (`sample` / `data` / `fit` / `hc`), the cross-join axes, the row count, and a compact preview — paralleling `print.ssdsims_scenario()` from `ssd-define-scenario`. A compound `ssd_scenario_tasks()` bundles all four into an `ssdsims_task_set` (the canonical expansion entry point, §1/§2).
- Add a baseline **runner** that loops over the four derived tables with `purrr::pmap()` (sample → data → fit → hc), threading each step's output to the next by looking up the parent result via its id foreign key, and returning the collected results. The runner does **no** task expansion of its own — it consumes `ssd_scenario_tasks()`.
- **Scope guards (explicit non-goals for this step):** no RNG and no `(seed, primer)` columns (those arrive in `task-primer` / `state-primitives`); no shards or `partition_by` grouping (that arrives in `task-tables`); no `targets` dependency; no Hive partitioning; no Parquet I/O. The runner executes in-process, in order, for a small scenario.
- This is **additive**: `ssd_run_scenario()` and the existing `ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` pipeline are untouched.

## Capabilities

### New Capabilities
- `task-lists`: Deriving the four per-step task tables (`sample`, `data`, `fit`, `hc`) from an `ssdsims_scenario` — the cross-join axes per step (with the draw/truncation split that keeps `nrow` off the draw), the per-step column contracts, the path-style `<step>_id` keys and parent foreign keys, the `ci = FALSE` collapse honoured at expansion, the `ssdsims_tasks` S3 class and its `print()` method, the compound `ssd_scenario_tasks()` / `ssdsims_task_set` bundle, and the `purrr::pmap()` baseline runner that executes the tables in dependency order. Establishes the task-table data shape that later roadmap steps refine.

### Modified Capabilities
<!-- None. This step is additive. It consumes the scenario-definition capability
     unchanged and adds no requirements to simulate-data / fit-distributions /
     hazard-concentrations / run-scenario / parallel-safe-seeding. Later steps
     (task-primer, state-primitives, task-tables) modify these. -->

## Impact

- **New code**: `R/task-lists.R` (the four derivation functions, the compound `ssd_scenario_tasks()`, the `ssdsims_tasks` / `ssdsims_task_set` constructors + `print()` methods, the path-style id / foreign-key helpers, and the baseline runner); `tests/testthat/test-task-lists.R`.
- **APIs**: New exports `ssd_scenario_sample_tasks()`, `ssd_scenario_data_tasks()`, `ssd_scenario_fit_tasks()`, `ssd_scenario_hc_tasks()`, `ssd_scenario_tasks()`, `ssd_run_scenario_baseline()`, plus `print.ssdsims_tasks()` / `print.ssdsims_task_set()` S3 methods; new `NAMESPACE` entries and roxygen `man/` pages.
- **Dependencies**: None added — `purrr`, `tidyr`, `dplyr`, `chk` are already in `Imports`. No `targets` / `dqrng` dependency at this step.
- **Consumes**: The `ssdsims_scenario` object and its recorded `ci = FALSE` ignore flag from `ssd-define-scenario`.
- **Downstream**: Unblocks `state-primitives` (with `task-primer`); the task-table shape defined here is what `task-tables` later groups into shards and decorates with `(seed, primer)`.
