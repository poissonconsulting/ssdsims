## Why

The targets-based redesign (`TARGETS-DESIGN.md` §1) requires a purely **declarative** scenario object as its foundation: the shard set a pipeline expands to must be a pure function of the scenario, known when `_targets.R` is sourced. The current package has no first-class scenario object — `ssd_run_scenario()` takes loose arguments and immediately runs the pipeline. This step lands the constructor first (roadmap entry `ssd-define-scenario`, a DAG root with no dependencies) so the data shape is settled before any RNG, task-expansion, or targets machinery is built.

## What Changes

- Add a public S3 constructor `ssd_define_scenario(data, ..., nsim, nrow, rescale, est_method, nboot, ci, seed, ...)` returning an `ssdsims_scenario` object.
- The object stores **only declarative fields**: `seed` (scalar integer), `nsim`, `nrow`, dataset **names**, the `fit` and `hc` argument-vector grids, `partition_by`, and an optional `upload` spec. It performs **no** RNG seeding, **no** task expansion, and has **no** dependency on `targets`.
- Store `min_pmix` **by name** in the `fit` grid (not as a function value), symmetric with datasets: a character vector of names, or a function / list of functions whose name is derived by symbol capture. The registry that resolves a name back to a function is a later step (`min-pmix-registry`); only the name reference lives here.
- Add a tiny `ssd_data()` normaliser that validates the `Conc` column and tibble shape and is the single entry point through which input data is forwarded.
- Apply the `ci = FALSE` rule at construction time: when `ci = FALSE` is the only confidence-interval value, passing any bootstrap-only knob (`nboot`, `ci_method`, `parametric`) is **rejected with an error**, forcing the user to omit them or set `ci = c(FALSE, TRUE)`.
- Add an S3 `print.ssdsims_scenario()` method that renders the declarative fields.
- This is **additive**: `ssd_run_scenario()` and the existing pipeline functions are untouched by this step.

## Capabilities

### New Capabilities
- `scenario-definition`: The declarative `ssdsims_scenario` object — its constructor `ssd_define_scenario()`, the `ssd_data()` input normaliser, dataset and `min_pmix` name references, field validation, the construction-time `ci = FALSE` rejection rule, and the `print()` method.

### Modified Capabilities
<!-- None. This step is additive; existing simulate-data / fit-distributions /
     hazard-concentrations / run-scenario / parallel-safe-seeding specs are
     unchanged. Later roadmap steps (migrate-public-api, task-tables,
     cleanup-lecuyer) will modify them. -->

## Impact

- **New code**: `R/scenario.R` (constructor, `print`), `R/data.R` (or co-located `ssd_data()` normaliser); corresponding `tests/testthat/test-scenario.R`.
- **APIs**: New exports `ssd_define_scenario()`, `ssd_data()`, `print.ssdsims_scenario()`; new `NAMESPACE` entries and roxygen `man/` pages.
- **Dependencies**: None added — uses existing `chk` for validation. No `targets`/`dqrng` dependency introduced at this step.
- **Downstream**: Unblocks roadmap entries `task-list-loop-baseline` and `partition-by`, which consume the scenario object.
