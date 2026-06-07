## Why

`ci` is a scalar flag, not an hc axis, but the three bootstrap-only knobs it
gates (`nboot`, `ci_method`, `parametric`) remain in `task_axes("hc")`
unconditionally. When `ci = FALSE` those columns are canonically `NA`, so a
scenario can still name them in `partition_by`/`bundle` — the subset check
against `task_axes("hc")` passes — and the run silently produces a degenerate
shard partitioned on an all-`NA` axis (a Hive path segment like
`nboot=__HIVE_DEFAULT_PARTITION__`). The constructor already rejects these knobs
as *scenario arguments* under `ci = FALSE`; the *layout* knobs that consume the
same axes are the one remaining gap.

## What Changes

- The constructor SHALL reject a bootstrap-only hc axis (`nboot`, `ci_method`,
  `parametric`) named in `partition_by$hc` or `bundle$hc` when `ci = FALSE`,
  aborting in the user-facing frame with a message that mirrors the existing
  bootstrap-knob guard (name the offending axis; direct the user to set
  `ci = TRUE` or drop it).
- `validate_partition_by()`/`validate_axis_list()` gain the scenario's `ci`
  value so the `hc`-step subset check can apply the `ci = FALSE` carve-out.
- No shape change: `task_axes("hc")` stays pure (independent of `ci`), the hc
  Parquet schema stays fixed-shape (`ci = FALSE` shards keep carrying the three
  bootstrap columns as `NA`), and the results-layout root is unchanged. This is
  deliberately the validation-only option (B), not a `ci`-dependent vocabulary.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities
- `scenario-definition`: extend the `partition_by`/`bundle` validation
  requirement so that, when `ci = FALSE`, a bootstrap-only hc axis named as a
  path axis (`partition_by$hc`) or an inner axis (`bundle$hc`) aborts — the
  layout-knob counterpart of the existing "bootstrap knobs rejected when
  `ci = FALSE`" requirement.

## Impact

- **Code**: `R/scenario.R` — `validate_partition_by()` and
  `validate_axis_list()` gain a `ci` parameter; the `ssd_define_scenario()` call
  site at the `validate_partition_by()` invocation passes `ci`. The generic
  unknown-axis path is unchanged; this adds one `ci = FALSE`-only branch on the
  `hc` step.
- **Tests**: `tests/testthat/` — new cases asserting that `nboot`/`ci_method`/
  `parametric` in `partition_by$hc` or `bundle$hc` abort under `ci = FALSE` and
  are accepted under `ci = TRUE`; existing `ci = TRUE` partitioning behaviour
  unchanged.
- **No** change to `task_axes()`, the partition split, the per-task primer,
  shard schema, runners, `ssd_summarize()`, the manifest, or the layout root.
- **Roadmap**: an off-DAG prose bullet in TARGETS-DESIGN.md §12 (an independent
  tidy-up with no dependants, alongside `scalar-ci-flag`).
