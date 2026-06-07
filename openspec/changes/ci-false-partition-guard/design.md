## Context

`ci` is a scenario-wide scalar flag (`scalar-ci-flag`, §1.2), excluded from
`task_axes("hc")`. The bootstrap-only knobs it gates (`nboot`, `ci_method`,
`parametric`) *do* remain in `task_axes("hc")` unconditionally; under
`ci = FALSE` they are carried as canonically `NA` columns.

The constructor already guards these as scenario *arguments*
(`R/scenario.R:251-271`: "Bootstrap-only knob … cannot be set when
`ci = FALSE`"). The *layout* knobs that consume the same axes —
`partition_by`/`bundle`, validated by `validate_partition_by()` /
`validate_axis_list()` (`R/scenario.R:354-478`) — are not guarded: their only
hc-step check is the generic `subset(task_axes("hc"))` test, which `nboot` et al.
pass. So `ssd_define_scenario(ci = FALSE, partition_by = list(hc = "nboot"))` is
accepted and yields a shard partitioned on an all-`NA` axis (a degenerate
single-cell Hive path).

## Goals / Non-Goals

**Goals:**
- Reject a bootstrap-only hc axis named in `partition_by$hc`/`bundle$hc` when
  `ci = FALSE`, in the user-facing frame, mirroring the existing knob guard.
- Keep the change surgical and local to the constructor's validation.

**Non-Goals:**
- A `ci`-dependent task-table or partition *shape* (option C). `task_axes("hc")`
  stays pure; the hc Parquet schema stays fixed-shape; the results-layout root
  (`layout=hash(partition_by)`) is unchanged.
- Any change to the runners, shard construction, primer, `ssd_summarize()`, or
  the manifest.

## Decisions

- **Validation guard (option B), not a `ci`-dependent vocabulary (option C).**
  The only concrete defect is partitioning/bundling on an all-`NA` axis; a
  validation guard fixes exactly that. Option C (dropping the bootstrap columns
  from `task_axes("hc")` when `ci = FALSE`) would make `task_axes()`
  scenario-dependent — threading `ci` through the primer, the split, the print
  method, and the runners — and would make the hc Parquet schema differ between
  `ci = FALSE` and `ci = TRUE` runs that share one layout root, forcing `ci`
  into the layout-root hash to avoid `duckplyr` schema-union conflicts. Far more
  surface area for a cosmetic gain. Rejected.
- **Thread `ci` into the validator, carve out only the `hc` step.** Add a `ci`
  parameter to `validate_partition_by()` (passed at the `R/scenario.R:284` call
  site) and to `validate_axis_list()`. When `isFALSE(ci)`, after the generic
  subset check, reject any `intersect(axes, c("nboot", "ci_method",
  "parametric"))` for `step == "hc"`. The carve-out is additive — it runs
  alongside the existing checks, not instead of them.
- **Reuse the existing guard's wording.** The new abort names the offending
  axis/axes and directs the user to set `ci = TRUE` or drop it, matching the
  argument-level guard so the two read as one coherent `ci = FALSE` contract.
- **Home the requirement on the `ci` scalar-flag requirement.** The spec delta
  extends "ci is a scalar flag selecting bootstrap confidence intervals" (which
  already owns the argument-level guard) rather than the generic `partition_by`
  requirement, keeping all `ci = FALSE` guards together.

## Risks / Trade-offs

- [A user *wants* an all-`NA` hc partition for layout uniformity across mixed
  `ci` runs] → Not a real use case: the layout root already keys on
  `partition_by`, and an all-`NA` path cell carries no information. The guard
  only fires under `ci = FALSE`; `ci = TRUE` partitioning is unaffected.
- [Error-message frame leakage] → `validate_partition_by`/`validate_axis_list`
  already abort with `call = call` (the user-facing frame) via plain loops, per
  the error-call-origin rule; the new branch follows the same pattern.

## Open Questions

None. Scope and approach are settled (option B, per the proposing discussion).
