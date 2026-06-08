## Why

`ssd_define_scenario()` currently accepts dataset input as a data frame or a list of data frames only — a documented gap versus `ssd_run_scenario()`, which also accepts a `fitdists` object, a `tmbfit` object, a generator function, and a function-name string (the `ssd_sim_data()` S3 methods). The targets redesign (`TARGETS-DESIGN.md` §1.1) needs the declarative scenario to express every input the legacy pipeline can. This step closes that gap; its only prerequisite (`ssd-define-scenario`) has landed, and it now also builds on `task-primer` / `local-dqrng-state` (see below).

## What Changes

- Extend `ssd_data()` (and therefore `ssd_define_scenario()`, which routes bare input through it) to accept the remaining input types `ssd_run_scenario()` handles today, in addition to data frames: a `fitdists` object, a `tmbfit` object, a generator **function**, and a **function-name string**.
- Treat each non-data-frame input as a data *generator* and **materialise it once, at construction**, to a validated tibble stored inline in `scenario$data` under its derived name. Datasets are tiny — generated ones included — so the scenario object is the transport; a generated dataset is reproducible *by being kept*, not by regeneration. Downstream consumers (#80's `ssd_run_scenario_baseline()` reading `scenario$data[[dataset]]`, the task tables, the future `registry`) see a plain data frame, so the collection stays homogeneous and **no** inert descriptor and **no** baseline abort-guard are needed.
  - **Reconciled with `task-list-loop-baseline` (#80 + fold).** The dataset name is the `dataset` cross-join axis of every task table (`sample`/`fit`/`hc`); because generators are materialised at construction, the baseline runner draws from them exactly as from data-frame inputs. No new task-table columns — only an additional dataset behind the existing `dataset` axis.
- Add a dot-prefixed **`.seed` argument to `ssd_data()`** (default `NULL`) that seeds generation **independently of the scenario**: each generator runs under a scoped `local_dqrng_state(.seed, task_primer(list(dataset = name)))`, with the dataset **name as the dqrng stream**, so one `.seed` fans out reproducibly across all named generators. Generation seeding plays no part in the per-task simulation primers.
- Enforce a **dqrng-only, pure-generator contract** via a post-hoc RNG-state check: base-R RNG movement always aborts ("use dqrng"); dqrng movement with `.seed = NULL` aborts ("supply `.seed`"); a pure generator needs no seed. The run is scoped, so construction leaves global `.Random.seed` unchanged. Supplying `.seed` when the call has no generators aborts.
- The `Conc` validation contract continues to apply to the materialised tibble of every input (data-frame or generator).

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability rather than
     introducing a new one. -->

### Modified Capabilities
- `scenario-definition`: The dataset-input requirements (`ssd_data()` assembly/normalisation and `ssd_define_scenario()` dataset input) are extended to accept data *generators* — `fitdists`, `tmbfit`, a function, or a function-name string — which are **materialised once at construction** to inline tibbles, seeded by a dedicated `ssd_data(.seed)` under a dqrng-only contract, alongside the existing data-frame forms.

## Impact

- **New code**: generator handling in `R/data.R` (the `ssd_data()` collector gains `.seed`, a `classify_input()` that validates and materialises each element, and the post-hoc RNG-state check), and `R/scenario.R` (the `scenario_dataset_names()` routing). Reuses `local_dqrng_state()` / `task_primer()` for scoped, name-streamed generation. New tests in `tests/testthat/test-data.R` / `test-scenario.R` / `test-task-lists.R`.
- **APIs**: No new exports. `ssd_data()` gains a `.seed` argument and accepts the wider input set; `ssd_define_scenario()` accepts the same through it. Roxygen/`man/` updates for the widened input contract, the `.seed` semantics, and the dqrng-only generator rule.
- **Dependencies**: Now depends on `task-primer` and `local-dqrng-state` (for scoped, name-streamed generation) in addition to `rlang` (symbol capture) and `chk` (validation) — a new edge versus the original RNG-free plan. Builds on #80 (`task-list-loop-baseline`): the `dataset` axis and the baseline runner.
- **Downstream**: A generated dataset is a realised tibble on the scenario, so `task-list-loop-baseline` runs it directly. The later `registry` step adds Parquet persistence; `dataset-provenance` (deferred) adds name-only regeneration and provenance metadata. The `ssd_run_scenario()` legacy path is untouched.
