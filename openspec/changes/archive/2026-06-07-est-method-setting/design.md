## Context

`est_method` is an hc cross-join axis today: `task_axes("hc")` (`R/task-lists.R:379`)
lists it, and `ssd_hc_sims()` (`R/hc-sims.R:75-110`) crosses the input with
`expand_grid(nboot, est_method, ci_method, parametric)`, calling
`ssdtools::ssd_hc()` once per row via `hc_seed()` → `hc_state()`
(`R/internal.R:95-169`). Each call re-runs the bootstrap, seeded only by
`(sim, stream, seed)`.

Session benchmarking established the waste and, crucially, *why the collapse is
safe*. Holding seed and inputs fixed and sweeping `est_method ∈ {arithmetic,
geometric, multi}` against **every** `ci_method`:

| Quantity | Across est_methods |
| --- | --- |
| bootstrap `samples` | **byte-identical** |
| `se` / `lcl` / `ucl` (the CI) | **est_method-invariant** |
| `est` (point estimate) | differs (analytical, RNG-free) |

So the *only* est_method-varying output is the analytical point estimate `est`
— the same quantity that `ci = FALSE` returns without any bootstrap. The CI is a
function of `(nboot, ci_method, parametric)` alone. `ssd_hc()` accepts only a
scalar `est_method` and exposes no public re-aggregation entry point for retained
`samples`, so the collapse must be done in ssdsims.

This mirrors the already-correct treatment of `proportion` (a vector consumed
within one `ssd_hc()` call, absent from `task_axes("hc")`) and the
`dists-simulation-setting` re-classification — but unlike the latter it removes a
*real* fan-out and recomputation, not just a label.

## Goals / Non-Goals

**Goals:**
- Make `est_method` an hc simulation setting: absent from `task_axes("hc")`, not
  a `partition_by`/`bundle` axis, stored at `scenario$hc$est_method`, allowed to
  be a vector without multiplying tasks.
- Compute every requested `est_method` from a **single** bootstrap per
  `(nboot, ci_method, parametric)` cell.
- ~3× cost reduction on the est_method axis. Point estimates (`est`) are
  unchanged (analytical, seed-independent); bootstrap CIs are **re-seeded** (see
  Decision 4) and change numerically — statistically equivalent, and now
  consistent across est_methods within a task.

**Non-Goals:**
- No change to `nboot`, `ci_method`, `parametric`, or any fit axis.
- No upstream `ssdtools` changes (a re-aggregation helper is noted as a future
  option, not required here).
- No change to the RNG primer scheme, shard layout, or Parquet I/O beyond
  dropping `est_method` from the hc path/inner vocabulary.

## Decisions

### Decision 1: Split the hc result into an est_method-invariant CI and a per-method analytical estimate
For each `(fit, nboot, ci_method, parametric)` cell, run the bootstrap **once**
(with `samples = TRUE`) to obtain the est_method-invariant `se`/`lcl`/`ucl`
(and, when retained, `samples`). Then obtain each requested method's point
estimate with a cheap analytical `ssd_hc(fit, ci = FALSE, est_method = m)` call
(no bootstrap, RNG-free). Assemble one output row per `est_method`: its own
`est` joined to the shared CI columns. Emit the rows in the order of the
requested `est_method` vector.

*Why:* the empirical table above proves the CI is est_method-invariant for every
ci_method and the bootstrap draw is identical, so this reproduces the per-method
`ci = TRUE` output exactly while bootstrapping once. The point estimate is the
same analytical value `ci = FALSE` already computes.

*Alternatives considered:*
- *Call `ssd_hc()` once per est_method (status quo, just regrouped):* still
  re-bootstraps per method — no saving. Rejected.
- *Re-aggregate the retained `samples` in-package under each est_method:*
  duplicates ssdtools' averaging logic and is fragile to upstream changes; the
  analytical `ci = FALSE` recompute gives the identical `est` for free without
  reimplementing aggregation. Rejected as unnecessary.
- *Upstream `ssdtools` helper `ssd_hc_resummarise(samples, est_method)`:*
  cleanest long-term, but cross-package and out of scope; the in-package split
  needs no upstream change.

### Decision 2: Drive the collapse off the single bootstrap with the first requested method
Run the one bootstrap with the first element of the (validated, de-duplicated)
`est_method` vector. Because the CI is invariant, the choice does not affect
output; using a requested method avoids an extra distinct configuration.

### Decision 3: Keep `est_method` vector-valued and validated, but as a setting
Validation is unchanged (character, non-`NA`, unique, subset of
`ssdtools::ssd_est_methods()`, length ≥ 1). It moves out of the `expand_grid()`
in `ssd_hc_sims()`/`ssd_fit_dists_sims` plumbing and the hc grid in
`ssd_define_scenario()` into the settings block, and is removed from
`task_axes("hc")` and the accepted `partition_by`/`bundle` hc vocabulary.

### Decision 4: The correctness gate is a same-seed invariant, not old-vs-new equality
The per-task RNG primer hashes the hc-grid row, which today **includes**
`est_method` (`R/task-primer.R:91-94`). Removing `est_method` from
`task_axes("hc")` changes that hash, so every hc task is **re-seeded** relative
to the old per-`est_method` path. The collapsed pipeline's bootstrap CIs are
therefore **not** byte-comparable to the pre-change output, and the byte-identity
seen in exploration holds only *at a fixed seed* — it cannot be asserted
post-hoc against the old pipeline. What is invariant is the point estimate `est`
(analytical, seed-independent); the CIs change numerically (and become
consistent across est_methods within a task).

The gate is accordingly a **unit-level, same-seed invariant** plus the
exploration record:
- *Exploration* (`exploration/est-method-invariance.R`): at a fixed seed, for
  every `ci_method`, the three est_methods share a byte-identical bootstrap draw
  and an est_method-invariant CI; only `est` differs. This is the justification
  that `est_method` need not be a bootstrap axis.
- *Unit test*: with a single fixed primer, the collapsed computation
  (one bootstrap → per-method analytical `est` + shared CI) is byte-identical to
  calling `ssdtools::ssd_hc(est_method = m)` once per method **seeded with that
  same primer**. This guards the invariance assumption against future `ssdtools`
  changes without claiming old-pipeline equality.

## Risks / Trade-offs

- **CI ceases to be est_method-invariant in a future ssdtools** → the same-seed
  invariant unit test (Decision 4) fails loudly; if that ever happens the
  fallback is to bootstrap per distinct `(est_method, ci_method)` only where
  coupling exists. The split is the optimisation; the test is the guard.
- **Re-seeding changes CI values vs. the old pipeline** → expected, not a
  regression: point estimates are unchanged and the new CIs are statistically
  valid and internally consistent. There is no byte-identity to preserve across
  the axis change, so no migration of stored CI values is implied.
- **Extra analytical `ci = FALSE` calls per method** → these are bootstrap-free
  and ~milliseconds; negligible beside the bootstrap they replace (net ~3×
  faster for a 3-method axis).
- **Snapshot / row-count churn** → hc task-count and printed-scenario snapshots
  that assumed an `est_method` fan-out must be re-recorded; `hc` *tibble* row
  counts (one row per method) are unchanged.
- **Ships with `dists-simulation-setting` in one PR (rebased, ordered).** Both
  changes edit `scenario-definition`'s "Constructor arguments are grouped by
  role" and the `ssd_define_scenario()` signature. Rather than collide at
  archive time, this delta is **rebased onto the `dists` end-state**: its
  role-grouping requirement already has `dists` removed from the axes clause and
  named in the settings block, so the two deltas **compose**. (Per PR review the
  non-`ci`-gated settings `dists`/`est_method`/`proportion` — valid when
  `ci = FALSE` — precede `ci`, and the scenario options `ci` gates follow it (the bootstrap
  axes `nboot`/`ci_method`/`parametric` and `samples`):
  `… range_shape2, dists, est_method, proportion, ci, nboot, ci_method,
  parametric, samples, partition_by …`.) `dists-simulation-setting`
  **owns** the behaviour-preserving signature reorder + call-site sweep
  groundwork (it establishes the contiguous settings block); this change only
  slots `est_method` into that block on top of the real bootstrap-collapse work.
  **Archive order: `dists-simulation-setting` first, then this change** — the
  last-synced delta wins the requirement text, and only this (rebased) delta
  carries both options moved.

## Migration Plan

1. Land the spec deltas (`hazard-concentrations`, `task-lists`,
   `scenario-definition`).
2. Implement: drop `est_method` from `task_axes("hc")`; rework
   `ssd_hc_sims()`/`hc_state()` to bootstrap once and attach per-method analytical
   estimates; move `est_method` to the settings block/storage in
   `ssd_define_scenario()`; update `partition_by`/`bundle` vocabulary and print.
3. Add the byte-identity regression test (gate).
4. Sweep call sites/docs and re-record snapshots.

No rollback data concerns — results are unchanged, so reverting the commit fully
restores prior behaviour.

## Open Questions

- Should ssdsims later contribute an upstream `ssdtools` re-aggregation helper so
  the point estimate need not be recomputed analytically? (Optimisation only; not
  needed for correctness or the headline 3× win.)
- Does any installed `ssdtools` version in CI break the est_method-invariance
  assumption? The regression test answers this per CI run.
