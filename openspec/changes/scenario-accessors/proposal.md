## Why

The original §1.1 design routed datasets and `min_pmix` through an *implicit registry* — a `tar_target` that resolved each referenced name once per project (datasets to a Parquet file, `min_pmix` to a pinned function value). But the scenario already **materialises its datasets inline** (`scenario-input-types` realises every input to a validated tibble at construction, with `ssd_data()` enforcing the `Conc` invariant), and the scenario is a referenced global in `_targets.R` that is transported to workers. So there is nothing to *register* or *persist*: a shard body just needs to **isolate a value by name from the scenario object** — a one-line accessor.

The same applies to `min_pmix` once we materialise it the way we materialise datasets: store the resolved function on the scenario, keyed by its name, at construction. The per-task hash continues to use the **name only** (so task identities stay stable under recompilation/JIT — the whole §1.1 concern), while the function value simply rides on the scenario for execution, exactly as the dataset bytes do. With both datasets and `min_pmix` materialised-on-scenario and accessed by name, the "registry" concept disappears entirely. This change therefore **replaces the registry with accessors** (and is renamed from `registry` to `scenario-accessors`); the Parquet I/O the registry was going to introduce moves to `task-tables`, which needs `duckplyr` anyway.

## What Changes

- **Materialise `min_pmix` on the scenario.** Extend `ssd_define_scenario()` so that, alongside the `min_pmix` *names* it already stores (used for hashing and the task path), it stores the resolved single-argument **functions** keyed by name — provided functions kept under their derived name, name-string references resolved to functions at construction (failing fast on an unresolvable name). Hashing/identity stays name-only.
- Add `scenario_dataset(scenario, name)` — isolate the materialised dataset tibble for `name` from the scenario, erroring informatively on an unknown name. (Datasets are already validated and materialised at construction; no registration or `Conc` re-check here.)
- Add `scenario_min_pmix(scenario, name)` — isolate the materialised `min_pmix` function for `name`.
- Rewire `resolve_min_pmix()` to the accessor (`scenario_min_pmix()`), dropping the runtime `ssdtools`/global-env lookup — resolution now happens once, at construction, so cluster workers get the function off the scenario with no reliance on the interactive global env.
- **Drop** the registry concept: no `ssd_register_dataset()` / `ssd_register_min_pmix()`, no `results/datasets/<name>.parquet`, no `_index.json`, no per-run pinned-function target. Name-only regeneration of *large* datasets remains the deferred `dataset-provenance` step (§12) — the only case where on-disk persistence is warranted.

## Capabilities

### New Capabilities
- `scenario-accessors`: materialise `min_pmix` functions on the scenario (keyed by name) and isolate both datasets and `min_pmix` by name via `scenario_dataset()` / `scenario_min_pmix()` accessors — replacing the implicit dataset/`min_pmix` registries with name-keyed access to already-materialised values.

### Modified Capabilities
- `scenario-definition`: `ssd_define_scenario()` additionally materialises the `min_pmix` functions on the scenario (keyed by name) at construction, where today it stores only the names and discards the functions after validation. The stored names (and thus task hashes) are unchanged.

## Impact

- **New code**: `scenario_dataset()`, `scenario_min_pmix()`, and the constructor change in `R/scenario.R` to materialise `min_pmix` functions; `resolve_min_pmix()` in `R/task-lists.R` rewired to the accessor. Tests in `tests/testthat/test-scenario.R` / `test-accessors.R`.
- **APIs**: New exports `scenario_dataset()` and `scenario_min_pmix()`; `ssd_define_scenario()` gains materialised `min_pmix`. Roxygen/`man/` and a `_pkgdown.yml` reference group.
- **Dependencies**: **none added** — values are read off the scenario; `min_pmix` identity is name-keyed (`identical()` for a materialisation sanity check). The previously-planned `arrow`/`duckplyr` and `digest` do **not** belong here: `duckplyr` (Parquet I/O, the team preference) is introduced by `task-tables`; `digest` by `manifest`.
- **Rename**: the `registry` change/roadmap node becomes `scenario-accessors`; TARGETS-DESIGN.md §1.1 (implicit registries) and §12 (DAG + roadmap bullet) are updated to the accessor framing. No glossary entry for "registry" exists, so none is removed.
- **Downstream**: `task-tables` reads datasets via `scenario_dataset()` and `min_pmix` via `scenario_min_pmix()`. **Depends on** `scenario-input-types` (datasets materialised inline). Independent of `manifest`.
