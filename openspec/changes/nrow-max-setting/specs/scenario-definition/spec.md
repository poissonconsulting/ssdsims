## ADDED Requirements

### Requirement: nrow_max sets the shared sample draw size
`ssd_define_scenario()` SHALL accept `nrow_max` as a scalar whole number
(default a reasonably high value, `1000L`), validated with
`chk::chk_whole_number`, stored on the scenario, and used as the **fixed**
size of the shared `sample` draw — replacing the previously derived
`max(scenario$nrow)`. `nrow_max` is a **simulation setting**: it SHALL NOT be a
cross-join axis, SHALL NOT enter any `task_axes(step)` or the per-task RNG
primer, and SHALL NOT be carried as a task-row column. The effective per-dataset
draw size SHALL be `min(nrow_max, nrow(data))` for `replace = FALSE` (so a high
`nrow_max` yields the full permutation) and `nrow_max` for `replace = TRUE`; it
is resolved by the runner from `nrow_max` and the dataset, not stored per task.
Each `nrow` value SHALL be validated at construction against the effective draw
size — `nrow <= nrow(data)` for `replace = FALSE`, `nrow <= nrow_max` for
`replace = TRUE` — aborting in the user-facing frame otherwise. Because the draw
size no longer depends on `max(nrow)`, adding an `nrow` value (within the draw
size) SHALL NOT change the shared draw.

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
`ssd_define_scenario()` SHALL order its arguments by role so that arguments of the same kind are contiguous, in this sequence: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **cross-join axes** — the grid knobs that fan out over tasks (`nrow`, `replace`, `dists`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`, `nboot`, `est_method`, `ci_method`, `parametric`); (3) the **simulation settings** — the non-axis knobs consumed within each task (`nrow_max`, `proportion`, `ci`, `samples`); (4) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any knob absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — setting the shared draw size (`nrow_max`), fanning out within the task's output (`proportion`), or applied uniformly (`ci`, `samples`). The simulation settings SHALL be contiguous, so `nrow_max`, `proportion`, `ci`, and `samples` sit together after the last axis (`parametric`) rather than interleaved among the axes. The stored scenario fields and `print.ssdsims_scenario()` SHALL list the knobs in the same role order (axes first, then the simulation settings). (Cross-reference: the proposed `dists-simulation-setting` change also moves `dists` into this contiguous simulation-settings block; whichever lands second reconciles the exact intra-block order.)

#### Scenario: Simulation settings are contiguous and follow the axes
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `nrow_max`, `proportion`, `ci`, and `samples` SHALL appear adjacent to one another, after the last cross-join axis (`parametric`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`)

#### Scenario: Print groups the knobs by role
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the grid SHALL render the axes first and the simulation settings (`nrow_max`, `proportion`, `ci`, `samples`) together after them
