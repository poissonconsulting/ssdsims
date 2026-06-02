## Context

`ssd-define-scenario` landed the declarative `ssdsims_scenario` object (`TARGETS-DESIGN.md` §1): seed, knobs, dataset names, fit/hc argument grids, `partition_by`, and the recorded `ci = FALSE` ignore. Nothing consumes it yet. The roadmap (§12) makes `task-list-loop-baseline` the immediate next step on the `define → baseline` edge: turn the scenario into the **flat per-task data shape** that all later steps build on, plus a no-frills runner.

The §2 "What goes into the hash" discussion already specifies the per-step identity axes — data: `(dataset, sim, replace)`; fit: data-identity + fit-grid row; hc: fit-identity + hc-grid row (modulo the §1.2 collapse). This step materializes exactly those tables as plain tibbles, with **no** RNG/primer columns and **no** targets/shard machinery. The point is to freeze the data shape early so subsequent steps (`task-primer`, `state-primitives`, `task-tables`) each change one thing against a working baseline rather than introducing the shape and the shards and the RNG all at once.

## Goals / Non-Goals

**Goals:**
- Three derivation functions producing `data`, `fit`, and `hc` task tables (tibbles) from a scenario, one column per cross-join axis.
- Honour the §1.2 `ci = FALSE` collapse at hc-table expansion using the flag the scenario already records.
- A baseline runner: three `purrr::pmap()` loops, data → fit → hc, threading outputs forward, returning collected results.
- Establish the column contract that `task-tables` later decorates with `(seed, primer)` and groups into shards.

**Non-Goals:**
- No RNG, no `(seed, primer)`/`stream` columns (those land in `task-primer` / `state-primitives`).
- No `targets` dependency, no `tar_map`, no shards, no `partition_by` grouping (those land in `task-tables`).
- No Hive partitioning, no Parquet I/O (later steps).
- No sub-truncation behaviour for `nrow` (the §5 property; here `nrow` is merely carried as a column).
- No change to `ssd_run_scenario()` or the existing public pipeline.

## Decisions

- **Decision: three separate tables, not one wide join.** The pipeline has three steps with different fan-outs and (later) different shard axes (§5 `partition_by` defaults differ per step). Keeping `data`/`fit`/`hc` as distinct tibbles, each referencing its parent's identity columns, mirrors the §6 step structure and lets `task-tables` group each independently. *Alternative*: a single fully-joined table — rejected; it conflates the per-step shard boundaries and bloats rows.
- **Decision: `nrow` is a carried column, not an identity axis.** §2 is explicit that `nrow` is **not** in the task hash because every `nrow` is a sub-truncation of the same `n_max`-row draw (§5). To keep the baseline shape forward-compatible with that property, the data table is not multiplied by `nrow`; it carries `nrow` as data. *Alternative*: cross-join on `nrow` now — rejected; it would bake in a shape that `nrow-sub-truncation` must later tear out.
- **Decision: reuse the scenario's recorded `ci = FALSE` collapse.** The collapse is decided at construction time and recorded on the scenario (`ssd-define-scenario` §1.2). The hc derivation honours that flag rather than re-deriving the rule, keeping a single source of truth. *Alternative*: re-implement the collapse here — rejected; duplicates logic and risks drift.
- **Decision: `min_pmix` carried by name.** Per §1.1/§2, function-valued knobs are referenced by name so later hashing is stable. The fit table stores the name; value lookup is a later step's concern. *Alternative*: embed the function — rejected; defeats the name-only indirection.
- **Decision: `tidyr::expand_grid()` / `crossing()` for the cross-joins; `purrr::pmap()` for the runner.** Both are already in `Imports` and match the §12 bullet ("three `purrr::pmap()` loops"). The runner is intentionally the simplest thing that threads the three steps. *Alternative*: a `targets` plan — rejected; explicitly out of scope and a later step.
- **Decision: task tables are a classed tibble (`ssdsims_tasks`) with a `print()` method.** A raw tibble prints as an undifferentiated grid; a thin S3 subclass that records the step lets `print.ssdsims_tasks()` lead with the step name, the cross-join axes, and the task count before a compact row preview — matching `print.ssdsims_scenario()` from `ssd-define-scenario` so the two read consistently. Subclassing a tibble keeps every dplyr/tidyr verb working (they operate on the underlying tibble; the class is reattached or harmlessly dropped). *Alternative*: return a bare tibble — rejected; the user wants a class that prints nicely and the step/axis metadata has nowhere else to live. *Alternative*: a separate container object wrapping all three tables — rejected for this step; three independently-classed tables match the per-step shard boundaries the later `task-tables` step groups on.
- **Decision: the runner reuses the existing per-step operations** (`ssd_sim_data` / `ssd_fit_dists_sims` / `ssd_hc_sims` logic) without RNG seeding, executing tasks in plain order. The baseline does not need reproducibility yet; `state-primitives` introduces per-task seeding on top of this shape.

## Risks / Trade-offs

- **Shape churn in later steps** → Mitigated by deriving the axes directly from §2's hash contract, so `task-tables` adds columns (`seed`, `primer`) rather than reshaping. Snapshot tests pin the column contract so later changes are visible diffs.
- **`ci = FALSE` collapse divergence from the scenario** → Mitigated by consuming the scenario's recorded flag rather than re-deriving; a test asserts the hc table row count matches the §1.2 example grid.
- **Baseline runner is not reproducible (no seeding)** → Accepted and intentional for this step; documented as a non-goal. Reproducibility arrives with `state-primitives`. Tests for the runner assert structure/threading, not draw values.
- **`nrow` carried-but-unused could confuse readers** → Mitigated with a roxygen note and an inline comment pointing to §5 / the `nrow-sub-truncation` roadmap step.

## Migration Plan

Additive. New `R/task-lists.R` and tests; new exports + `man/` pages. No existing function changes, no data migration, no new dependency. The existing `ssd_run_scenario()` path is untouched and coexists. Fully reversible by removing the new file and its `NAMESPACE` entries.

## Open Questions

- Final exported names for the three derivation functions and the runner (e.g. `ssd_scenario_data_tasks()` / `_fit_tasks()` / `_hc_tasks()` and a `ssd_run_scenario_baseline()`), to be settled in implementation so they read consistently with the eventual `task-tables` `_shards` wrappers (§12).
- Whether the runner returns a list of three result tibbles or a single nested structure — resolve during implementation; the spec only requires that outputs thread data → fit → hc and are collected.
- Exact column naming for parent-identity columns on `fit`/`hc` rows (pref`dataset`/`sim`/`replace` carried verbatim vs. a compound key) — pick the form that `task-tables` can group on directly.
