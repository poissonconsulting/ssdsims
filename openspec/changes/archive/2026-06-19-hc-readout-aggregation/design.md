# Design: hc-readout-aggregation

## Context

`scenario-combine` ships the design factory `ssd_design_targets()`: it groups
members by `seed`, builds a synthetic design-reference scenario, unions the
members' per-step shard tables de-duplicated by cell, and addresses shards at
`<root>/seed=<value>/layout=<hash(partition_by)>/<step>/<cells>`. Naked cell
addressing requires that the same cell denote byte-identical content, which is why
the factory's `require_uniform()` rejects members of a seed-group that differ in
any byte-shaping setting it does not otherwise reconcile — including the four
**non-axis hc settings** `proportion`, `est_method`, `ci`, `samples`.

Those four are *seed-free readouts*: `ssdtools::ssd_hc()` already vectorizes
`proportion`/`est_method` over a single bootstrap (`hc_collapse_est_methods()`
gives one bootstrap, analytical `est` per method), and `ci`/`samples` are strict
supersets (`ci = TRUE`/`samples = TRUE` add columns; the point `est` is invariant
to `ci`). So they need not force distinct shards — they can be computed as a
maximal set per cell and filtered per member. This change replaces the abort with
that aggregation, **per shared cell over only the members that touch it**, so the
expensive bootstrap runs only where a `ci = TRUE` member has tasks.

## Goals / Non-Goals

**Goals:**

- Allow design members in a seed-group to differ in `proportion`/`est_method`/
  `ci`/`samples`, reconciling them into shared hc shards.
- Per-overlap aggregation (not global): `union` `proportion`/`est_method`, `any`
  `ci`/`samples`, over only the members whose task set contains a cell.
- Preserve byte-identity: a served `ci = FALSE` `est` equals the standalone value;
  a `ci = TRUE` member's CI uses its own cell primer; single-scenario results
  unchanged.
- No ssdtools refactor.

**Non-Goals:**

- Aggregating the **draw-shaping** hc axes (`nboot`/`ci_method`/`parametric`/
  `distset`). They stay cell axes (in the primer); folding them in would change the
  RNG stream.
- Relaxing the byte-shaping contract on `nrow_max`, the fit `dists` union, or
  `partition_by` — those remain `scenario-combine`'s `require_uniform()` aborts.
- A draws-shard / readout-layer split of the hc step (a larger refactor).

## Decisions

### Decision: per-task readout overrides on `ssd_run_hc_step()`, defaulting to the slice

`ssd_run_hc_step()` currently reads `proportion`/`est_method`/`ci`/`samples` from
the scenario slice and applies them uniformly to every task in the shard. This
change adds optional **per-task** demand for those four (carried as columns on the
shard's `tasks`, or `NULL` to fall back to the slice). When absent (the
single-scenario factory and the standalone runner), behaviour and bytes are
unchanged. When present (the design factory), each task is summarised with its
cell's aggregated demand. The runner still calls `hc_collapse_est_methods()` /
`ssdtools::ssd_hc()` with vector `est_method`/`proportion` and scalar `ci`/
`samples` — no ssdtools change.

*Alternative considered:* a separate design-only hc runner — rejected; duplicates
the runner and risks drift from the byte-identity oracle.

### Decision: reduce the union of members' hc tasks per cell

In `ssd_design_targets()`, drop `require_uniform()` for the four settings. Build
the union of the members' hc tasks (each tagged with its source member and that
member's readout demand) and group by the hc **cell** (`fit` identity +
`nboot`/`ci_method`/`parametric`/`distset`). For each group reduce the demand:
`union` `proportion`/`est_method`, `any` `ci`/`samples`. Attach the reduced demand
to the shard's tasks so the runner computes the maximal set; the existing
per-member summary filter (keyed by `hc_id`) then selects each member's rows.

### Decision: `ci` NA-collapse routing

A `ci = FALSE` task collapses `nboot`/`ci_method`/`parametric` to `NA`, so its cell
never coincides with a `ci = TRUE` task. Because the point `est` is analytical and
bootstrap-config-invariant, route a `ci = FALSE` task's `est` to a coincident
`ci = TRUE` shard at the same `(fit-id, distset)` when one exists; only mint a
standalone `ci = FALSE` shard where no `ci = TRUE` member overlaps. The computed hc
shards are therefore every `ci = TRUE` member's cells plus the `ci = FALSE` cells
with no overlapping `ci = TRUE` shard. The per-member summary maps each member's hc
tasks (including its `ci = FALSE` tasks) to the serving shard and filters.

*Alternative considered:* always mint a separate `ci = FALSE` shard even when a
`ci = TRUE` shard covers the cell — rejected; it recomputes the analytical `est`
that the `ci = TRUE` shard already carries (the wasted-overlap the change exists to
avoid).

## Risks / Trade-offs

- **Demand reduction must not change task identity** → the four settings are
  *non-axis* (absent from `task_axes(hc)`), so they never enter the primer;
  reducing them leaves `(seed, primer)` untouched and byte-identity intact.
- **Routing `ci = FALSE` to the wrong serving shard** → routing keys on
  `(fit-id, distset)` and relies on the analytical-`est` invariance the package
  already guarantees (`hc_collapse_est_methods()`'s `analytical_est`); a regression
  test pins a served `est` against the standalone `ci = FALSE` value.
- **Widening a design later recomputes the affected hc shards** (a new
  `proportion`/`est_method`, or the first `ci`/`samples = TRUE`, changes a cell's
  reduced demand) → accepted; `sample`/`fit` stay cached.

## Migration Plan

Builds on `scenario-combine` (land it first). Additive at the API surface
(`ssd_run_hc_step()` gains defaulted optional args); the only behaviour change is
that previously-rejected designs now run. The `scenario-combine` test asserting the
abort flips to assert success in this change.
