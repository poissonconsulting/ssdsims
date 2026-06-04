## Why

`ssd_hc()` always returns a `samples` list-column (the per-row bootstrap draws), populated only when called with `samples = TRUE`. The runners never passed it, so every result carried an empty (zero-length) `samples` column — an implicit, unspecified artifact rather than a deliberate choice. Retaining the draws is a real, occasionally-needed option (diagnostics, replay), but it is **output retention only**: it does not change the estimates, and it can be large (`nboot` draws per dist per task), so it must be opt-in and must not perturb the per-task RNG.

## What Changes

- Add a scalar logical `samples` knob to `ssd_define_scenario()` (default `FALSE`), stored at `scenario$hc$samples`, validated as a flag, and rendered by `print()` alongside the other hc knobs.
- Thread it to `ssd_hc()` via `hc_data_task()`/`hc_data_task_primer()` and both runners (baseline + per-shard), so `samples = TRUE` retains the bootstrap draws.
- Keep it **off** the grid and **off** the task identity / RNG primer: it is a length-1 superset flag (a single `TRUE` subsumes `FALSE`), and it does not change `est`/`lcl`/`ucl`. A change re-runs the hc step (the discarded draws must be re-bootstrapped) but yields byte-identical estimates; invalidation comes from `scenario` being the hc step's input, not from the hash.
- `ssd_summarize()` drops the `dists`/`samples` list-columns (projection pushdown) so the analysis-ready summary stays tidy; the draws remain in the per-shard hc Parquet.

## Capabilities

### Modified Capabilities
- `scenario-definition`: `ssd_define_scenario()` gains the scalar `samples` knob (default `FALSE`), stored on `scenario$hc` and passed through to `ssd_hc()` for output retention only.

## Impact

- **New code**: the `samples` argument + flag validation + storage in `R/scenario.R`; `samples` threaded through `hc_data_task()`/`hc_data_task_primer()` and the baseline + per-shard runners; `ssd_summarize()` projection. Tests in `tests/testthat/`.
- **APIs**: `ssd_define_scenario()` gains `samples = FALSE`. No new exports.
- **Dependencies**: none.
