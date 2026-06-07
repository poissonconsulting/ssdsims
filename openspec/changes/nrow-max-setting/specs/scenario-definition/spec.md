## ADDED Requirements

### Requirement: nrow_max sets the shared sample draw size
`ssd_define_scenario()` SHALL accept `nrow_max` as a scalar whole number
(default a reasonably high value, `1000L`), validated with
`chk::chk_whole_number`, stored on the scenario, and used as the **fixed**
size of the shared `sample` draw — replacing the previously derived
`max(scenario$nrow)`. `nrow_max` is a sample-level **simulation setting**: it
SHALL NOT be a cross-join axis, SHALL NOT enter any `task_axes(step)` or the
per-task RNG primer, and SHALL NOT be carried as a task-row column. The
effective per-dataset draw size SHALL be `min(nrow_max, nrow(data))` for
`replace = FALSE` (so a high `nrow_max` yields the full permutation) and
`nrow_max` for `replace = TRUE`; it is resolved by the runner from `nrow_max`
and the dataset, not stored per task. Each `nrow` value SHALL be validated at
construction against the effective draw size — `nrow <= nrow(data)` for
`replace = FALSE`, `nrow <= nrow_max` for `replace = TRUE` — aborting in the
user-facing frame otherwise. Because the draw size no longer depends on
`max(nrow)`, adding an `nrow` value (within the draw size) SHALL NOT change the
shared draw. `nrow_max` is **not** `ci`-gated: the draw happens regardless of
`ci`.

#### Scenario: nrow_max defaults high and is a whole number
- **WHEN** `ssd_define_scenario()` is called without `nrow_max`
- **THEN** the scenario SHALL store `nrow_max = 1000L`, and a non-whole-number `nrow_max` SHALL abort in the user-facing frame

#### Scenario: nrow is validated against the effective draw size
- **WHEN** `ssd_define_scenario(..., replace = TRUE, nrow = 50, nrow_max = 20)` is called (an `nrow` exceeding `nrow_max` under resampling)
- **THEN** the constructor SHALL abort in the user-facing frame, because `nrow` cannot exceed the shared draw size

#### Scenario: nrow_max is a simulation setting, not an axis
- **WHEN** the `sample` axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` never multiplies tasks, enters a primer, or becomes a task-row column

## MODIFIED Requirements

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **structural cross-join axes** (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`); (3) the **non-`ci`-gated simulation settings** — knobs that are valid and meaningful even when `ci = FALSE`: `nrow_max` (sample-level, the shared draw size), `dists` (fit-level), then `est_method` and `proportion` (hc-level, shaping the analytical point estimate); (4) `ci`, then the knobs it **gates** — the bootstrap **cross-join axes** `nboot`/`ci_method`/`parametric` (which `ci = FALSE` rejects) and the `samples` setting (which only retains bootstrap draws); (5) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any knob absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — setting the shared draw size (`nrow_max`), fanning out within the task's output (`est_method`, `proportion`), or applied uniformly (`ci`, `dists`, `samples`). `nrow_max` is the **sample**-level setting (the fixed shared-draw size, absent from `task_axes("sample")`); `dists` is the **fit**-level setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `est_method`/`proportion`/`ci`/`samples` are **hc**-level. `nrow_max`, `dists`, `est_method`, and `proportion` SHALL precede `ci` (none are `ci`-gated — the draw, the fit, and the analytical estimate all happen regardless of `ci`); the bootstrap-only knobs `nboot`/`ci_method`/`parametric` and `samples` SHALL follow `ci`. Storage SHALL remain step-based: `nrow_max` is stored at the sample level, `dists` at `scenario$fit$dists`, and the hc knobs at `scenario$hc` in signature order (`est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, `samples`). `print.ssdsims_scenario()` SHALL render each grid in that stored order (settings flagged), render `nrow_max` among the sample knobs, and render `dists` among the fit knobs, both marked as settings rather than axes.

#### Scenario: non-ci-gated settings precede ci; gated knobs follow it
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `nrow_max`, `dists`, `est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, and `samples` SHALL appear adjacent to one another in that order, after the last structural axis (`range_shape2`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`), so the non-`ci`-gated settings precede `ci` and the knobs it gates follow it

#### Scenario: nrow_max is a sample-level simulation setting
- **WHEN** the sample-step axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` is neither a path axis nor an inner axis and does not enter the per-task primer; it sets the shared draw size and is stored at the sample level

#### Scenario: Print groups the knobs by ci-gating
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render `est_method`, `proportion`, `ci`, then the bootstrap axes (`nboot`, `ci_method`, `parametric`), then `samples`, with the non-axis knobs flagged as settings; and the sample/fit knobs SHALL render `nrow_max` and `dists` marked as settings
