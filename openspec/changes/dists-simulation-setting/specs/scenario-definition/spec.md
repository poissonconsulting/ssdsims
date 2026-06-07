## MODIFIED Requirements

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role so that arguments of the same kind are contiguous, in this sequence: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **cross-join axes** — the grid knobs that fan out over tasks (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`, `nboot`, `est_method`, `ci_method`, `parametric`); (3) the **simulation settings** — the non-axis knobs consumed within each task (`dists`, `proportion`, `ci`, `samples`); (4) the **partitioning and remaining arguments** (`partition_by`, `bundle`). (The `upload` argument was removed from `ssd_define_scenario()` by the `cloud-upload` change — it is now a runner argument of `ssd_scenario_targets()` — so it is no longer part of this ordering.) A simulation setting is any knob absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — fanning out within the task's output (`proportion`) or applied uniformly (`dists`, `ci`, `samples`). `dists` is the **fit**-level simulation setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `proportion`/`ci`/`samples` are **hc**-level. The simulation settings SHALL be contiguous, with `dists` leading (fit before hc), so `dists`, `proportion`, `ci`, and `samples` sit together after the last axis (`parametric`) rather than `dists` being interleaved among the fit axes as it is today. Storage SHALL remain step-based: `dists` is stored at `scenario$fit$dists` and `proportion`/`ci`/`samples` at `scenario$hc`. The stored `scenario$hc` field and `print.ssdsims_scenario()` SHALL list the hc knobs in role order (hc axes, then the hc simulation settings `proportion`/`ci`/`samples`); `print.ssdsims_scenario()` SHALL render `dists` among the fit knobs, marked as a setting rather than an axis.

#### Scenario: Simulation settings are contiguous and follow the axes
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `dists`, `proportion`, `ci`, and `samples` SHALL appear adjacent to one another, after the last cross-join axis (`parametric`) and before the partitioning arguments (`partition_by`, `bundle`), with `dists` first

#### Scenario: dists is a simulation setting, not an axis
- **WHEN** the fit-step axis vocabulary (`task_axes("fit")`) is queried
- **THEN** it SHALL NOT contain `"dists"`, so `dists` is neither a path axis nor an inner axis and does not enter the per-task primer; it is applied uniformly to every fit task and stored at `scenario$fit$dists`

#### Scenario: Print groups the hc knobs by role
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render the axes first (`nboot`, `est_method`, `ci_method`, `parametric`) and the simulation settings (`proportion`, `ci`, `samples`) together after them, and the fit knobs SHALL render `dists` marked as a setting
