# scenario-definition Delta

## RENAMED Requirements

- FROM: `### Requirement: Configurable, validated partition_by knob with a complementary bundle entry point`
- TO: `### Requirement: Configurable, validated partition_by argument with a complementary bundle entry point`

- FROM: `### Requirement: est_method is an hc simulation setting computed from shared samples`
- TO: `### Requirement: est_method is an hc scenario setting computed from shared samples`

## MODIFIED Requirements

### Requirement: Path-axis vs inner-axis split
The package SHALL define, for each step, the inner (Parquet-column) axes as that step's axis vocabulary (`task_axes()`, #80) minus its `partition_by` path axes, and SHALL expose this split to downstream task-table and shard construction. The path axes determine the shard count for a step (`Π |path axis|`) and the **Hive shard path** (`path_key()` over the chosen path axes — distinct from the `<step>_id` task-identity key, which #80 keys over *all* axes and which `partition_by` does not change); the inner axes are carried as columns within each shard.

#### Scenario: Inner axes are the complement of path axes
- **WHEN** the fit step's vocabulary is queried for a scenario whose `fit` path axes are `c("dataset", "sim", "nrow", "rescale")`
- **THEN** the inner axes SHALL be the remaining fit axes (`replace`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`)

#### Scenario: All-axes-in-path yields no inner axes
- **WHEN** a step's `partition_by` lists every axis in that step's vocabulary
- **THEN** the inner-axis set for that step SHALL be empty (one task per shard — the shard path then equals the `<step>_id` task identity, the only case where they coincide)

#### Scenario: Per-step vocabularies match #80's task_axes()
- **WHEN** the axis vocabulary is queried per step
- **THEN** it SHALL equal `task_axes(step)`: `sample` = `dataset`, `sim`, `replace`; `fit` adds `nrow`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`; `hc` adds `nboot`, `ci_method`, `parametric` (`ci` and `est_method` are hc scenario settings, not axes — see *"ci is a scalar flag selecting bootstrap confidence intervals"* and *"est_method is an hc scenario setting computed from shared samples"*)

#### Scenario: Steps partition independently — no cross-step constraint
- **WHEN** a step's path axes are a valid subset of its own vocabulary but differ arbitrarily from its parent step's path axes (e.g. finer or coarser on a shared axis)
- **THEN** the constructor SHALL accept them without any parent-consistency check, since a child shard may span several parent shards (an m:n relationship resolved at the read layer, not by restricting `partition_by`); the `<parent>_id` foreign key remains well-defined regardless

### Requirement: ci is a scalar flag selecting bootstrap confidence intervals
`ssd_define_scenario()` SHALL accept `ci` as a scalar logical flag (a single non-`NA` `TRUE`/`FALSE`; default `FALSE`), validated with `chk::chk_flag`, stored at `scenario$hc$ci`, and passed to `ssdtools::ssd_hc()`. `ci` SHALL NOT be a grid axis and SHALL NOT enter the `hc` task identity (`task_axes("hc")`) or the per-task RNG primer: the point estimate `est` is invariant to `ci` (computed analytically from the fit, independent of the bootstrap and RNG), so a single `ci = TRUE` run is a superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is scenario-wide and either/or — `ci = FALSE` for cheap, bootstrap-free point estimates, or `ci = TRUE` for estimates plus confidence intervals. When `ci = FALSE`, supplying any bootstrap-only scenario option (`nboot`, `ci_method`, or `parametric`) SHALL abort in the user-facing frame, directing the user to set `ci = TRUE` or omit the option. `print.ssdsims_scenario()` SHALL render `ci` among the hc scenario options.

#### Scenario: ci defaults to FALSE and is a flag
- **WHEN** `ssd_define_scenario()` is called without `ci`
- **THEN** `scenario$hc$ci` SHALL be the scalar `FALSE`

#### Scenario: A vector ci is rejected
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (or any non-flag `ci`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame, because `ci` is a scalar flag

#### Scenario: Bootstrap axes rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` (or with `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort with an informative error stating that bootstrap-only scenario options are not allowed when `ci = FALSE`, and directing the user to set `ci = TRUE` or omit the option(s)

#### Scenario: ci = TRUE retains bootstrap axes
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap axes SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × ci_method × parametric` (with `est_method` summarised within each task, not fanned out)

#### Scenario: ci does not enter the hc task identity
- **WHEN** the `hc` axis vocabulary (`task_axes("hc")`) is queried
- **THEN** it SHALL NOT contain `"ci"`, so `ci` is neither a path axis nor an inner axis and does not change the per-task primer; it is applied uniformly to every hc task

### Requirement: est_method is an hc scenario setting computed from shared samples
`ssd_define_scenario()` SHALL accept `est_method` as an hc-level **scenario
setting** (default `"multi"`), validated as a non-`NA`, unique character vector
that is a subset of `ssdtools::ssd_est_methods()`, stored at
`scenario$hc$est_method`, and passed to the hc step. `est_method` SHALL NOT be a
cross-join axis: it SHALL be absent from `task_axes("hc")`, SHALL NOT enter the
`hc` task identity or the per-task RNG primer, and SHALL NOT be an accepted
`partition_by`/`bundle` axis for the `hc` step. A vector `est_method` SHALL NOT
multiply hc tasks; instead every requested method SHALL be summarised from the
**single** bootstrap sample set of each hc task (each method appears as a row in
that task's `hc` tibble). The role mirrors `proportion`: a within-result
dimension consumed inside one bootstrap, not a task multiplier.

#### Scenario: est_method defaults to multi and is stored as a setting
- **WHEN** `ssd_define_scenario()` is called without `est_method`
- **THEN** `scenario$hc$est_method` SHALL be `"multi"`, and `task_axes("hc")` SHALL NOT contain `"est_method"`

#### Scenario: A vector est_method does not multiply hc tasks
- **WHEN** `ssd_define_scenario(..., ci = TRUE, est_method = c("arithmetic", "geometric", "multi"))` is run
- **THEN** the hc task count SHALL NOT be multiplied by the number of `est_method` values, and each hc task's `hc` tibble SHALL contain one row per requested `est_method`, all derived from that task's single bootstrap sample set

#### Scenario: est_method is rejected as an hc partition axis
- **WHEN** `ssd_define_scenario(..., partition_by = list(hc = "est_method"))` (or the equivalent `bundle`) is called
- **THEN** the constructor SHALL abort, because `"est_method"` is not in the `hc` step's axis vocabulary

### Requirement: nrow_max sets the shared sample draw size
`ssd_define_scenario()` SHALL accept `nrow_max` as a scalar whole number
(default a reasonably high value, `1000L`), validated with
`chk::chk_whole_number`, stored on the scenario, and used as the **fixed**
size of the shared `sample` draw — replacing the previously derived
`max(scenario$nrow)`. `nrow_max` is a sample-level **scenario setting**: it
SHALL NOT be a cross-join axis, SHALL NOT enter any `task_axes(step)` or the
per-task RNG primer, and SHALL NOT be carried as a task-row column. The
effective per-dataset draw size SHALL be `min(nrow_max, nrow(data))` for
`replace = FALSE` (so a high `nrow_max` yields the full permutation) and
`nrow_max` for `replace = TRUE`; it is resolved by the runner from `nrow_max`
and the dataset, not stored per task. Because the fixed `nrow_max` draw is the
largest sample any `nrow` can sub-truncate, `nrow_max` is the **universal
ceiling** for `nrow`: each `nrow` value SHALL be a whole number in
`[5, nrow_max]` (5 being the fit floor), validated at construction, and a value
outside that range SHALL abort in the user-facing frame with a message that
**cites `nrow_max`'s value** so a raised ceiling is discoverable. Per-dataset
`replace = FALSE` infeasibility *within* that range — an `nrow` no greater than
`nrow_max` but greater than a dataset's row count, whose permutation draw caps
at `min(nrow_max, nrow(data))` — is **not** an error here; it is governed by the
`replace`-default requirement (a silent per-cell discard in task expansion).
`nrow_max` itself SHALL be a whole number of at least 5. Because the draw size
no longer depends on `max(nrow)`, adding an `nrow` value (within the draw size)
SHALL NOT change the shared draw. `nrow_max` is **not** `ci`-gated: the draw
happens regardless of `ci`.

#### Scenario: nrow_max defaults high and is a whole number
- **WHEN** `ssd_define_scenario()` is called without `nrow_max`
- **THEN** the scenario SHALL store `nrow_max = 1000L`, and a non-whole-number `nrow_max` SHALL abort in the user-facing frame

#### Scenario: nrow exceeding nrow_max aborts, citing nrow_max
- **WHEN** `ssd_define_scenario(..., nrow = 50, nrow_max = 20)` is called (an `nrow` above the fixed draw size, regardless of `replace`)
- **THEN** the constructor SHALL abort in the user-facing frame with a message citing `nrow_max`'s value (`= 20`), because no `nrow` can sub-truncate a draw it exceeds; and `ssd_define_scenario(..., nrow = c(10, 10000), nrow_max = 10000, replace = c(TRUE, FALSE))` SHALL instead succeed, since `nrow_max` admits `nrow = 10000`

#### Scenario: nrow_max is a scenario setting, not an axis
- **WHEN** the `sample` axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` never multiplies tasks, enters a primer, or becomes a task-row column

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **structural cross-join axes** (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`); (3) the **non-`ci`-gated scenario settings** — scenario options that are valid and meaningful even when `ci = FALSE`: `nrow_max` (sample-level, the shared draw size), `dists` (fit-level), then `est_method` and `proportion` (hc-level, shaping the analytical point estimate); (4) `ci`, then the scenario options it **gates** — the bootstrap **cross-join axes** `nboot`/`ci_method`/`parametric` (which `ci = FALSE` rejects) and the `samples` setting (which only retains bootstrap draws); (5) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A scenario setting is any scenario option absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — setting the shared draw size (`nrow_max`), fanning out within the task's output (`est_method`, `proportion`), or applied uniformly (`ci`, `dists`, `samples`). `nrow_max` is the **sample**-level setting (the fixed shared-draw size, absent from `task_axes("sample")`); `dists` is the **fit**-level setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `est_method`/`proportion`/`ci`/`samples` are **hc**-level. `nrow_max`, `dists`, `est_method`, and `proportion` SHALL precede `ci` (none are `ci`-gated — the draw, the fit, and the analytical estimate all happen regardless of `ci`); the bootstrap-only scenario options `nboot`/`ci_method`/`parametric` and `samples` SHALL follow `ci`. Storage SHALL remain step-based: `nrow_max` is stored at the sample level, `dists` at `scenario$fit$dists`, and the hc scenario options at `scenario$hc` in signature order (`est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, `samples`). `print.ssdsims_scenario()` SHALL render each grid in that stored order (settings flagged), render `nrow_max` among the sample scenario options, and render `dists` among the fit scenario options, both marked as settings rather than axes.

#### Scenario: non-ci-gated settings precede ci; gated scenario options follow it
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `nrow_max`, `dists`, `est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, and `samples` SHALL appear adjacent to one another in that order, after the last structural axis (`range_shape2`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`), so the non-`ci`-gated settings precede `ci` and the scenario options it gates follow it

#### Scenario: nrow_max is a sample-level scenario setting
- **WHEN** the sample-step axis vocabulary (`task_axes("sample")`) is queried
- **THEN** it SHALL NOT contain `"nrow_max"`, so `nrow_max` is neither a path axis nor an inner axis and does not enter the per-task primer; it sets the shared draw size and is stored at the sample level

#### Scenario: Print groups the scenario options by ci-gating
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render `est_method`, `proportion`, `ci`, then the bootstrap axes (`nboot`, `ci_method`, `parametric`), then `samples`, with the non-axis scenario options flagged as settings; and the sample/fit scenario options SHALL render `nrow_max` and `dists` marked as settings

### Requirement: samples retains the bootstrap draws (output only)
`ssd_define_scenario()` SHALL accept a scalar logical `samples` argument (default `FALSE`), validated as a flag (a single non-`NA` `TRUE`/`FALSE`), stored at `scenario$hc$samples`, and passed to `ssdtools::ssd_hc()` so that `samples = TRUE` retains the per-row bootstrap draws in the hc `samples` list-column. `samples` SHALL NOT be a grid axis and SHALL NOT enter the task identity (`task_axes("hc")`) or the per-task RNG primer: it does not change the estimates, so changing it SHALL yield byte-identical `est`/`lcl`/`ucl` while re-running the hc step to populate (or empty) the `samples` column. `print.ssdsims_scenario()` SHALL render `samples` among the hc scenario options.

#### Scenario: samples defaults to FALSE and is stored
- **WHEN** `ssd_define_scenario()` is called without `samples`
- **THEN** `scenario$hc$samples` SHALL be `FALSE`

#### Scenario: samples = TRUE retains draws without changing estimates
- **WHEN** a scenario is run with `samples = TRUE` versus `FALSE` (same seed, `ci = TRUE`)
- **THEN** the hc estimates SHALL be byte-identical, and the `samples` list-column SHALL be populated only when `samples = TRUE`

#### Scenario: samples must be a flag
- **WHEN** `ssd_define_scenario(..., samples = c(TRUE, FALSE))` (or any non-flag) is called
- **THEN** the constructor SHALL abort in the user-facing frame
