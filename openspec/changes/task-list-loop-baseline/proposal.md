## Why

The targets redesign (`TARGETS-DESIGN.md` §1–§2) expands a declarative `ssdsims_scenario` into a flat set of per-task rows — one row per cross-join cell of each pipeline step — that later steps shard, seed, and route through `targets`. `ssd-define-scenario` (the DAG root, now landed) gives us the declarative object but nothing consumes it yet. This step turns the scenario into the **task-table data shape** the rest of the roadmap is built on, plus a trivial runner, so every subsequent step (`task-primer`, `state-primitives`, `task-tables`, shards, Hive partitioning) can swap in one piece at a time against a known-good baseline. It is the roadmap entry immediately after `ssd-define-scenario` (`define → baseline` in the §12 DAG) and is now unblocked.

## What Changes

- Add task-list derivation that expands an `ssdsims_scenario` into **three task tables** (tibbles):
  - `data` tasks — one row per `(dataset, sim, replace)` cross-join cell.
  - `fit` tasks — each data-task identity crossed with the `fit` arg grid (`rescale`, `computable`, `at_boundary_ok`, `min_pmix` name, `range_shape1`, `range_shape2`).
  - `hc` tasks — each fit-task identity crossed with the `hc` arg grid (`nboot`, `est_method`, `ci_method`, `parametric`), respecting the construction-time `ci = FALSE` collapse already recorded on the scenario (§1.2).
- One column per cross-join axis; the tables are the canonical record of "what work exists". `nrow` is carried on data tasks as an ordinary column but is deliberately **not** an identity axis (the §5 sub-truncation property; not exercised here).
- Add a baseline **runner** that is three `purrr::pmap()` loops (data → fit → hc), threading each step's output to the next, returning the collected results.
- **Scope guards (explicit non-goals for this step):** no RNG and no `(seed, primer)` columns (those arrive in `task-primer` / `state-primitives`); no shards or `partition_by` grouping (that arrives in `task-tables`); no `targets` dependency; no Hive partitioning; no Parquet I/O. The runner executes in-process, in order, for a small scenario.
- This is **additive**: `ssd_run_scenario()` and the existing `ssd_sim_data()` / `ssd_fit_dists_sims()` / `ssd_hc_sims()` pipeline are untouched.

## Capabilities

### New Capabilities
- `task-lists`: Deriving the three per-step task tables (`data`, `fit`, `hc`) from an `ssdsims_scenario` — the cross-join axes per step, the per-step column contracts, the `ci = FALSE` collapse honoured at expansion, and the `purrr::pmap()` baseline runner that executes the tables in dependency order. Establishes the task-table data shape that later roadmap steps refine.

### Modified Capabilities
<!-- None. This step is additive. It consumes the scenario-definition capability
     unchanged and adds no requirements to simulate-data / fit-distributions /
     hazard-concentrations / run-scenario / parallel-safe-seeding. Later steps
     (task-primer, state-primitives, task-tables) modify these. -->

## Impact

- **New code**: `R/task-lists.R` (the three derivation functions + the baseline runner); `tests/testthat/test-task-lists.R`.
- **APIs**: New exports for the task-derivation functions and the baseline runner (names finalised in design); new `NAMESPACE` entries and roxygen `man/` pages.
- **Dependencies**: None added — `purrr`, `tidyr`, `dplyr`, `chk` are already in `Imports`. No `targets` / `dqrng` dependency at this step.
- **Consumes**: The `ssdsims_scenario` object and its recorded `ci = FALSE` ignore flag from `ssd-define-scenario`.
- **Downstream**: Unblocks `state-primitives` (with `task-primer`); the task-table shape defined here is what `task-tables` later groups into shards and decorates with `(seed, primer)`.
