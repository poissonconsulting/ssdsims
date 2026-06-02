## Why

`ssd_define_scenario()` currently accepts dataset input as a data frame or a list of data frames only — a documented gap versus `ssd_run_scenario()`, which also accepts a `fitdists` object, a `tmbfit` object, a generator function, and a function-name string (the `ssd_sim_data()` S3 methods). The targets redesign (`TARGETS-DESIGN.md` §1.1, §12 roadmap entry `scenario-input-types`) needs the declarative scenario to express every input the legacy pipeline can, while staying serialisable to a tiny manifest. This step closes that gap; its only prerequisite (`ssd-define-scenario`) has landed.

## What Changes

- Extend `ssd_data()` (and therefore `ssd_define_scenario()`, which routes bare input through it) to accept the remaining input types `ssd_run_scenario()` handles today, in addition to data frames: a `fitdists` object, a `tmbfit` object, a generator **function**, and a **function-name string**.
- Treat each non-data-frame input as a data *generator*. The constructor SHALL derive a dataset name (by argument name, else symbol capture — the same rule used for data frames) and record the generator **by name and kind**, storing **no** function bodies and **no** `fitdists`/`tmbfit` payloads. This keeps the scenario declarative and its per-task hash stable (§1.1).
  - **Depends on `task-list-loop-baseline` (#80).** A generator-backed dataset is an entry on the **data (dataset) axis** of the task tables #80 builds (`ssd_scenario_data_tasks()` cross-joins dataset names); the descriptor SHALL carry what that data-axis derivation needs to key a dataset. The descriptor shape **and the task breakdown in `tasks.md` SHALL be reconciled with #80 when it merges** — both are provisional until then.
- Represent a generator-backed dataset in the `ssdsims_data` collection and the scenario's `datasets` as a small declarative descriptor (name + generator kind + the captured reference), distinct from an inline data-frame dataset. The `Conc` validation contract continues to apply to data-frame inputs; generator inputs are validated structurally (a name is derivable; a function-name string resolves to a function; a function takes the expected argument) without being run.
- Materialisation of generator-backed datasets to actual data is **out of scope**: it is the job of the later targets-only `dataset-registry` change (§1.1), which realises synthetic datasets at registration time. Until then, generator descriptors are carried but not executed.
- Update `print.ssdsims_scenario()` / the `ssd_data()` print path so a generator-backed dataset renders its kind (e.g. `<fitdists>`, `<fn>`) rather than a row/column summary.

## Capabilities

### New Capabilities
<!-- None: this extends the existing scenario-definition capability rather than
     introducing a new one. -->

### Modified Capabilities
- `scenario-definition`: The dataset-input requirements (`ssd_data()` assembly/normalisation and `ssd_define_scenario()` dataset input) are extended to accept data *generators* — `fitdists`, `tmbfit`, a function, or a function-name string — recorded declaratively by name and kind, alongside the existing data-frame forms.

## Impact

- **New code**: generator-input handling in `R/data.R` (the `ssd_data()` collector and the `ssd_data_validate()`/name-derivation helpers) and `R/scenario.R` (the `scenario_dataset_names()` routing and the descriptor stored in `datasets`); print-path updates; new tests in `tests/testthat/test-data.R` / `test-scenario.R`.
- **APIs**: No new exports. `ssd_data()` and `ssd_define_scenario()` accept a wider set of inputs; the `datasets` field gains a generator-descriptor form. Roxygen/`man/` updates for the widened input contract.
- **Dependencies**: None added — reuses `rlang` (symbol capture) and `chk` (validation). No `targets`/`dqrng` dependency at this step.
- **Downstream**: Unblocks `dataset-registry` (§12), which consumes the generator descriptors to materialise datasets. The `ssd_run_scenario()` legacy path is untouched.
