## Why

`ssd_define_scenario()` currently accepts dataset input as a data frame or a list of data frames only — a documented gap versus `ssd_run_scenario()`, which also accepts a `fitdists` object, a `tmbfit` object, a generator function, and a function-name string (the `ssd_sim_data()` S3 methods). The targets redesign (`TARGETS-DESIGN.md` §1.1; the `scenario-input-types` entry in `ROADMAP.md`) needs the declarative scenario to express every input the legacy pipeline can.

Exploration on this branch (see `exploration/reprexes-and-api.md` and `exploration/ssd-gen-reprex.md`) reframed *how* to close that gap. Two findings drove the redesign:

- **A generator materialised once is a *fixture the scenario resamples*, not the legacy "fresh data per sim" design.** The scenario's `sample` step draws `slice_sample(data, n = max(nrow), replace)` from whatever tibble it holds, so a materialised generator is indistinguishable downstream from a data frame — and this change's own design defers provenance (`dataset-provenance`), storing no generator descriptor. So the only real job is *produce one reproducible fixture tibble*, which is a **generation** concern cleanly separable from **scenario definition**.
- **Generation belongs in the dqrng world, not `ssd_sim_data()`'s.** `ssd_sim_data()` seeds with L'Ecuyer-CMRG and (verified empirically) ignores its `seed=` argument for generator methods — so reusing it cannot satisfy a reproducible, seedable generation contract. A dedicated helper drawing directly under dqrng can.

## What Changes

- **Rename `ssd_data()` → `ssd_scenario_data()`.** The current name collides with the unrelated `ssdtools::ssd_data(x)` (which masks it whenever `ssdtools` is attached). `ssd_scenario_data()` pairs with `ssd_define_scenario()` and is free of `ssdtools`/`ssddata` exports. It remains the assembly/validation collection constructor — a homogeneous named list of validated `Conc` tibbles (class `ssdsims_data`).
- **Add `ssd_gen(..., .n, .seed)`** — a new helper that accepts **only** generator-style inputs (a function, a function-name string, a `fitdists`, or a `tmbfit`) and materialises each, once, to a validated `Conc` tibble of `.n` rows. It returns a classed `ssdsims_gen` collection designed to be spliced into `ssd_scenario_data()`:
  - `ssd_scenario_data(boron = ccme_boron, ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))` (an `ssdsims_gen` argument is flattened in), or
  - `ssd_scenario_data(boron = ccme_boron, !!!ssd_gen(synth = ssd_rlnorm, .n = 30, .seed = 42))` (rlang splice — no magic).
  - `.seed` and `.n` are **required** dot-prefixed formals (never absorbed into `...`, never partial-matched from a `seed=`/`n=` dataset name), making irreproducible or unsized generation impossible by construction.
  - Each generator is materialised under a scoped `local_dqrng_state(.seed, task_primer(list(dataset = name)))` — the dataset **name** as the dqrng stream, `.seed` as the base seed — so a single `.seed` fans out reproducibly across all named generators on independent streams. Construction leaves the global `.Random.seed` unchanged.
  - A `data.frame` is rejected by `ssd_gen()` (it belongs directly in `ssd_scenario_data()`); `fitdists`/`tmbfit` dispatch most-specific-first and resolve to the matching `ssd_r<dist>` draw via `ssdtools::estimates()`; a function-name string resolves a **bare name** via `get0()`/`match.fun()` (no `eval(parse())`).
- **`ssd_define_scenario()` accepts dataset input ONLY as an `ssd_scenario_data()` collection.** The bare-data-frame, bare-list, and `name=` convenience forms are dropped — naming and validation live entirely in `ssd_scenario_data()`/`ssd_gen()`. This shrinks `scenario_datasets()` (no `data_expr` capture, no list routing) and, because generation now happens *before* construction, **`ssd_define_scenario()` stays RNG-free** (the existing "No side effects on RNG state" requirement is preserved, not violated).
- **Depend on `task-rng-postcheck`.** `ssd_gen()` seeds each generator through `local_dqrng_state()`, which brackets the draw with `task-rng-postcheck`'s `chk_dqrng_backend_intact()` witness that each draw actually came from dqrng — reusing that change's shared, tested machinery instead of a bespoke RNG-state check. With `.seed` required, the witness reduces to "drew from dqrng (reproducible) or pure (reproducible) → ok; escaped dqrng → abort". (`dqrng` remains an `Imports` dependency, per `task-rng-postcheck` as landed; no `Suggests`/`dqrng_usable()` gate.)
- The `Conc` validation contract continues to apply to every materialised tibble (data-frame or generator).

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability rather than
     introducing a new one. -->

### Modified Capabilities
- `scenario-definition`: the dataset-input requirements are extended — `ssd_data()` is renamed `ssd_scenario_data()`; `ssd_define_scenario()` accepts only that collection (no bare/`name=` forms); and a new `ssd_gen()` materialises generator inputs (`fitdists`/`tmbfit`/function/function-name string) once to inline `Conc` tibbles, seeded reproducibly by a required `.seed` with the dataset name as the dqrng stream, under a dqrng-only contract reusing `task-rng-postcheck`.

## Impact

- **New code**: `ssd_gen()` and its generator-type dispatch (`R/gen.R` or `R/data.R`); the `ssdsims_gen` class and its splice/flatten handling in `ssd_scenario_data()`. **Renamed**: `ssd_data()` → `ssd_scenario_data()` (and its class stays `ssdsims_data`). **Simplified**: `scenario_datasets()` drops the bare-frame/list/`name=` branches. Reuses `local_dqrng_state()`/`task_primer()` (scoped, name-streamed generation) and `chk_dqrng_backend_intact()` (from `task-rng-postcheck`). New tests in `tests/testthat/test-data.R`/`test-scenario.R`/`test-task-lists.R`.
- **APIs**: `ssd_data()` is **renamed** to `ssd_scenario_data()` (breaking, but the package is pre-1.0 / `0.0.0.9xxx`); a new `ssd_gen()` is exported; `ssd_define_scenario()` loses its `name=` argument and accepts only an `ssd_scenario_data()` collection. Roxygen/`man/`/`NAMESPACE`/`_pkgdown.yml`/README/vignette updated for the rename and the new helper.
- **Dependencies**: depends on `task-rng-postcheck` (`chk_dqrng_backend_intact()`, bracketed into `local_dqrng_state()`) — now landed on `main` (which kept `dqrng` in `Imports`) — plus `task-primer`/`local-dqrng-state` for scoped name-streamed generation, in addition to `rlang` (symbol capture, splicing) and `chk` (validation). The dqrng/`task_primer` edge lives in `ssd_gen()` only — the scenario constructor keeps the inert, RNG-free contract `ssd-define-scenario` established.
- **Downstream**: a generated dataset is a realised tibble in the `ssd_scenario_data()` collection, so `task-list-loop-baseline` (#80) runs it directly. The later `registry` step persists it; `dataset-provenance` (deferred) adds name-only regeneration and provenance metadata. The legacy `ssd_run_scenario()` / `ssd_sim_data()` path is untouched.
