## Why

The `scenario-definition` spec (`Constructor arguments are grouped by role`)
lists `dists` among the **cross-join axes** — "the grid axes that fan out over
tasks". The implementation disagrees: `task_axes("fit")` does **not** contain
`dists` (`R/task-lists.R:363`). `dists` is a single character vector passed
*whole* and *uniformly* to one `ssdtools::ssd_fit_dists()` call per fit task
(`R/task-lists.R:256`); it never fans out, enters a per-task **primer**, or
becomes a **partition** level. Fanning out per-distribution would dissolve the
model averaging that *defines* one fit, so `dists` is deliberately not an axis.

By the glossary's own definition, `dists` is therefore a **scenario setting**
(absent from `task_axes(step)`, consumed within each task, applied uniformly —
the same category as `ci` and `samples`). It is the *fit*-level setting, where
`proportion`/`ci`/`samples` are hc-level. The spec's role-grouping requirement
both mis-labels it as an axis and, consequently, positions it in the fit-axis
block of the signature rather than with the other scenario settings.

This was surfaced while correcting the related TARGETS-DESIGN.md §9 limitation
(*"`dists` and `nboot` are not fit/hc grid axes"*), whose heading was wrong
about `nboot`: `nboot` **is** an hc axis (`task_axes("hc")`). The design-doc and
glossary corrections land directly; this change carries the matching
`scenario-definition` **spec** correction and the signature move, per the
OpenSpec workflow.

## What Changes

- **Re-classify `dists` as a fit-level scenario setting** in the
  `Constructor arguments are grouped by role` requirement: remove it from the
  cross-join-axes clause (2) and name it in the simulation-settings clause (3),
  alongside `proportion`/`ci`/`samples`. The taxonomy stays binary: a scenario
  option is either an axis (in `task_axes(step)`) or a scenario setting.
- **Move `dists` in the `ssd_define_scenario()` signature** out of the fit-axis
  block to the contiguous simulation-settings block, leading it (fit setting
  before the hc settings): `… parametric, dists, proportion, ci, samples,
  partition_by, …`. All these formals follow `...` and are name-only at the
  call site, so the reorder is behaviour-preserving.
- **Keep storage step-based**: `dists` remains stored at `scenario$fit$dists`
  (it shapes the fit). `print.ssdsims_scenario()` renders it among the fit
  scenario options but marked as a setting, not an axis.
- **Sweep call sites** (`canonical-call-sites` convention) so examples,
  fixtures, tests, scripts, vignettes, and `inst/targets-templates/` pass the
  constructor arguments in the new signature order.

No change to the task graph, primers, shard layout, or any computed result:
`dists` was never an axis in code. This change makes the spec, the signature,
and the docs agree with the implementation.

## Capabilities

### New Capabilities
<!-- None: this corrects/extends the existing scenario-definition capability. -->

### Modified Capabilities
- `scenario-definition`: The `Constructor arguments are grouped by role`
  requirement is corrected so `dists` is a fit-level **scenario setting**
  (not a cross-join axis) and sits in the contiguous simulation-settings block
  of the signature, while remaining stored under `scenario$fit`.

## Impact

- **Spec**: `openspec/specs/scenario-definition/spec.md` — the role-grouping
  requirement and its `Scenario settings are contiguous` / `Print groups the
  hc scenario options by role` scenarios.
- **Code**: `R/scenario.R` (signature reorder; `print.ssdsims_scenario()` if it
  positions `dists`). No change to `R/task-lists.R` (`task_axes()` already
  excludes `dists`).
- **Call sites**: examples in `@examples`, `tests/`, `scripts/`, `vignettes/`,
  `inst/targets-templates/` reordered to signature order (name-only args).
- **Docs**: `man/` regenerated; `GLOSSARY.md` and `TARGETS-DESIGN.md` already
  corrected ahead of this change (the conceptual classification); the glossary's
  "moving `dists` … lands via the `dists-scenario-setting` change" note
  resolves when this change is applied.
- **Risk**: low — behaviour-preserving signature reorder of name-only formals;
  no result, primer, or shard changes. Snapshot tests that print a scenario may
  need re-recording.
