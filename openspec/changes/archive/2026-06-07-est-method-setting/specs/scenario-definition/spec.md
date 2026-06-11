## ADDED Requirements

### Requirement: est_method is an hc simulation setting computed from shared samples
`ssd_define_scenario()` SHALL accept `est_method` as an hc-level **simulation
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

## MODIFIED Requirements

### Requirement: ci is a scalar flag selecting bootstrap confidence intervals
`ssd_define_scenario()` SHALL accept `ci` as a scalar logical flag (a single non-`NA` `TRUE`/`FALSE`; default `FALSE`), validated with `chk::chk_flag`, stored at `scenario$hc$ci`, and passed to `ssdtools::ssd_hc()`. `ci` SHALL NOT be a grid axis and SHALL NOT enter the `hc` task identity (`task_axes("hc")`) or the per-task RNG primer: the point estimate `est` is invariant to `ci` (computed analytically from the fit, independent of the bootstrap and RNG), so a single `ci = TRUE` run is a superset of `ci = FALSE` (same `est`, plus the `se`/`lcl`/`ucl` columns). The choice is scenario-wide and either/or — `ci = FALSE` for cheap, bootstrap-free point estimates, or `ci = TRUE` for estimates plus confidence intervals. When `ci = FALSE`, supplying any bootstrap-only scenario option (`nboot`, `ci_method`, or `parametric`) SHALL abort in the user-facing frame, directing the user to set `ci = TRUE` or omit the option. `print.ssdsims_scenario()` SHALL render `ci` among the hc scenario options.

#### Scenario: ci defaults to FALSE and is a flag
- **WHEN** `ssd_define_scenario()` is called without `ci`
- **THEN** `scenario$hc$ci` SHALL be the scalar `FALSE`

#### Scenario: A vector ci is rejected
- **WHEN** `ssd_define_scenario(..., ci = c(FALSE, TRUE))` (or any non-flag `ci`) is called
- **THEN** the constructor SHALL abort in the `ssd_define_scenario()` frame, because `ci` is a scalar flag

#### Scenario: Bootstrap-only scenario options rejected when ci = FALSE
- **WHEN** `ssd_define_scenario(..., ci = FALSE, nboot = 1000)` (or with `ci_method`/`parametric`) is called
- **THEN** the constructor SHALL abort with an informative error stating that bootstrap-only scenario options are not allowed when `ci = FALSE`, and directing the user to set `ci = TRUE` or omit the option(s)

#### Scenario: ci = TRUE retains the bootstrap axes
- **WHEN** `ssd_define_scenario(..., ci = TRUE, nboot = c(100, 1000), ci_method = "weighted_samples")` is called
- **THEN** the bootstrap axes SHALL be retained and no error SHALL be emitted; the hc step fans out over `nboot × ci_method × parametric` (with `est_method` summarised within each task, not fanned out)

#### Scenario: ci does not enter the hc task identity
- **WHEN** the `hc` axis vocabulary (`task_axes("hc")`) is queried
- **THEN** it SHALL NOT contain `"ci"`, so `ci` is neither a path axis nor an inner axis and does not change the per-task primer; it is applied uniformly to every hc task

### Requirement: Constructor arguments are grouped by role
`ssd_define_scenario()` SHALL order its arguments by role: (1) the required data/`seed`/`nsim` inputs and the dataset `name`; (2) the **structural cross-join axes** (`nrow`, `replace`, `rescale`, `computable`, `at_boundary_ok`, `min_pmix`, `range_shape1`, `range_shape2`); (3) the **non-`ci`-gated simulation settings** — scenario options that are valid and meaningful even when `ci = FALSE`: `dists` (fit-level), then `est_method` and `proportion` (hc-level, shaping the analytical point estimate); (4) `ci`, then the scenario options it **gates** — the bootstrap **cross-join axes** `nboot`/`ci_method`/`parametric` (which `ci = FALSE` rejects) and the `samples` setting (which only retains bootstrap draws); (5) the **partitioning and remaining arguments** (`partition_by`, `bundle`, `upload`). A simulation setting is any scenario option absent from `task_axes(step)`: it never multiplies tasks, but is consumed inside each task — fanning out within the task's output (`est_method`, `proportion`) or applied uniformly (`ci`, `dists`, `samples`). `dists` is the **fit**-level setting (a single character vector handed whole to every fit task's `ssd_fit_dists()` call, absent from `task_axes("fit")`); `est_method`/`proportion`/`ci`/`samples` are **hc**-level. `est_method` and `proportion` SHALL precede `ci` (a `ci = FALSE` scenario still selects an `est_method` and `proportion` for its analytical estimate); the bootstrap-only scenario options `nboot`/`ci_method`/`parametric` and `samples` SHALL follow `ci`. Storage SHALL remain step-based: `dists` is stored at `scenario$fit$dists` and the hc scenario options at `scenario$hc` in signature order (`est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, `samples`). `print.ssdsims_scenario()` SHALL render each grid in that stored order (settings flagged) and render `dists` among the fit scenario options, marked as a setting rather than an axis.

#### Scenario: non-ci-gated settings precede ci; gated scenario options follow it
- **WHEN** the `ssd_define_scenario()` signature is inspected
- **THEN** `dists`, `est_method`, `proportion`, `ci`, `nboot`, `ci_method`, `parametric`, and `samples` SHALL appear adjacent to one another in that order, after the last structural axis (`range_shape2`) and before the partitioning arguments (`partition_by`, `bundle`, `upload`), so the non-`ci`-gated settings precede `ci` and the scenario options it gates follow it

#### Scenario: dists is a simulation setting, not an axis
- **WHEN** the fit-step axis vocabulary (`task_axes("fit")`) is queried
- **THEN** it SHALL NOT contain `"dists"`, so `dists` is neither a path axis nor an inner axis and does not enter the per-task primer; it is applied uniformly to every fit task and stored at `scenario$fit$dists`

#### Scenario: Print groups the hc scenario options by ci-gating
- **WHEN** an `ssdsims_scenario` is printed
- **THEN** the hc grid SHALL render `est_method`, `proportion`, `ci`, then the bootstrap axes (`nboot`, `ci_method`, `parametric`), then `samples`, with the non-axis scenario options flagged as settings; and the fit scenario options SHALL render `dists` marked as a setting
