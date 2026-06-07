## Context

Two threads converge in this change.

**The carried-column / slice split.** The values a step's runner consumes that
are *not* cross-join axes split across two mechanisms today:

- **On the task row** (carried columns, excluded from `task_axes()`): `n_max`
  on every `sample` task (the shared draw size), `ci` on every `hc` task.
- **In the scenario slice** (`scenario_step_slice()`, hashed into each shard's
  `targets` command): `dists` (fit), `proportion`/`samples` (hc).

The split is historical, not principled — the `scalar-ci-flag` design says so
outright. Both routes are visible in one function: `ssd_run_hc_step()` reads
`proportion`/`samples` from the slice but `ci = t$ci` from the row.

**The derived draw size and its churn.** `sample_task_grid()` sets
`n_max = max(scenario$nrow)`; the `sample` step draws `n_max` rows once per
`(dataset, sim, replace)` and `fit` truncates `head(sample, nrow)` (§5). Because
`n_max` is derived from `max(nrow)` and stored on the `sample` row, **widening
`nrow` re-draws**: the `task-shards` requirement *"Extending nrow does not
produce a stale sample draw"* exists precisely to propagate that re-draw through
the per-child edge to the affected `fit` shards. The `sample` and `fit` steps
are cheap relative to the hc bootstrap (`cost-estimation`: the bootstrap
dominates), so coupling the draw size to the `nrow` axis buys churn for nothing.

Neither `n_max` nor `ci` is in `task_axes()`, so wherever they live they never
touch `task_primer()`, `path_key()`, or `partition_by` — this is a
storage/plumbing change, not an identity one.

## Goals / Non-Goals

**Goals:**

- Add an explicit `nrow_max` simulation setting (the fixed shared-draw size,
  high default) replacing the derived `n_max = max(nrow)`, so extending `nrow`
  (within the draw size) never re-draws.
- Complete *Direction B*: move the last two carried columns (`n_max`, `ci`) off
  the task tables into the scenario slice, so a task row is purely identity.
- Keep per-task results reproducible and order-independent; keep the §5
  `head(., nrow)` prefix property.

**Non-Goals:**

- No change to task identity / primer / partitioning (`n_max`/`ci` were never
  axes).
- No change to the point-estimate computation or to which knobs are axes.
- *Direction A* ("everything on the row") — explicitly rejected (below).
- Removing the `seed`/`primer` columns the shard path attaches per row — they
  are genuinely per-task (the primer is hashed from identity) and stay.

## Decisions

### Decision: `nrow_max` is a scenario-level simulation setting, default high

`nrow_max` is a single `chk_whole_number`, default `1000L`, stored on the
scenario and placed among the non-`ci`-gated simulation settings of
`ssd_define_scenario()` — leading them as the sample-level setting, before
`dists` (fit) and `est_method`/`proportion` (hc), and ahead of `ci` (it is not
`ci`-gated; the draw happens regardless). It is **not** an axis (absent from
`task_axes()`), so it never multiplies tasks or enters a primer.

*Rationale:* the draw + fit are cheap, so a generous fixed draw costs almost
nothing in compute (the fit still operates on `head(draw, nrow)` = `nrow` rows,
not the full draw — only the *draw* and the `sample`-shard *storage* scale with
`nrow_max`). Pinning the size to a scenario constant is what decouples it from
the `nrow` axis. `1000L` comfortably exceeds realistic SSD sample sizes (species
counts rarely exceed ~100) so `nrow` can grow freely without re-draw.

*Alternative considered — keep deriving `n_max = max(nrow)` but stabilise it
some other way.* Rejected: any derivation from the `nrow` axis re-draws when the
axis grows; only an axis-independent setting removes the coupling.

### Decision: the effective draw size is `min(nrow_max, nrow(data))` for `replace = FALSE`

You cannot draw more distinct rows than a dataset has, so for `replace = FALSE`
the realised draw is `D = min(nrow_max, nrow(data))` per dataset — with a high
default this is the **full permutation**, itself the maximal no-churn draw (any
`nrow <= nrow(data)` is a `head()` prefix of it). For `replace = TRUE` the draw
is `nrow_max` rows. Crucially **`D` is computed in the runner** from `nrow_max`
(scenario slice) and `nrow(data)` (dataset, already in the slice), so it need
*not* be a row column — which is exactly what lets `n_max` leave the row.

Construction-time validation accordingly checks each `nrow` against the
effective draw size: `nrow <= nrow(data)` for `replace = FALSE` (as today) and
`nrow <= nrow_max` for `replace = TRUE`.

*Alternative considered — abort when `nrow_max > nrow(data)` for
`replace = FALSE`.* Rejected: that would force the user to pick a per-dataset
`nrow_max`, defeating "one high default". Silent capping to the full permutation
is the natural, churn-free behaviour. (Flagged in Open Questions.)

### Decision: `n_max` and `ci` move off the task tables into the slice (Direction B)

`sample_task_grid()` stops emitting `n_max`; `hc_grid_tbl()` stops emitting the
`ci` column. `scenario_step_slice()` carries `nrow_max` on the `sample` slice and
`ci` on the `hc` slice (joining `proportion`/`samples`). The runners read from
the slice: `ssd_run_sample_step()` computes `D` from `nrow_max` + the dataset
(was `t$n_max`); `ssd_run_hc_step()` reads `ci` from the slice (was `t$ci`); the
baseline runner reads both from `scenario$…` as it already does for
`proportion`/`samples`/`dists`. The `ci = FALSE ⟹ bootstrap-only knobs NA`
canonicalisation in `hc_grid_tbl()` stays — it keys off the scalar `ci` (now read
from the scenario), it just no longer emits a `ci` column.

*Rationale:* one uniform rule — *a simulation setting is never a row column;
identity is all a row holds* — matching the GLOSSARY definition of "simulation
setting". Smaller, purely-identity shard rows; `nrow_max`/`ci` stored once.

*Alternative considered — Direction A (everything on the row): make
`proportion`/`samples`/`dists` carried columns too.* Rejected: `proportion` and
`dists` are vector-valued, so they would become **constant list-columns repeated
on every row** (Parquet bloat, awkward shape — `proportion` *fans out within* a
task's output, it is not per-row data). The slice stores each setting once and is
the established vehicle; Direction B is the lower-storage, more principled end
state.

### Decision: slice membership shifts invalidation as expected

With `nrow_max` on the `sample` slice, changing `nrow_max` invalidates the
`sample` slice → rebuilds `sample` and downstream (correct: the draw changed).
With `ci` on the `hc` slice, changing `ci` rebuilds only `hc`/`summary`
(consistent with the existing *"Changing an hc-only knob rebuilds only hc"*
contract). Extending `nrow` within the draw size now leaves the `sample` shard
**cached** and mints only new `nrow`-keyed `fit` shards — the old "widen
`max(nrow)` re-draws the sample shard" scenario is replaced by this stronger
caching guarantee. The `task-shards` "keep the fold, no `data` step" requirement
keeps its conclusion, but its *stale-short-draw* rationale is now structurally
moot (the draw size no longer grows with `nrow`).

## Risks / Trade-offs

- **Numeric re-baseline for a fixed `seed`.** → The realised `sample` draw is now
  `D` rows (not `max(nrow)`), changing the RNG consumption and therefore sampled
  data + everything downstream. Intended and pre-1.0; the `head(., nrow)` prefix
  property is preserved within a fixed draw, and snapshots re-record in the same
  change. (`replace = FALSE` drawing the full permutation vs. a `max(nrow)`
  prefix likewise changes the realised draw length.)
- **`sample`-shard storage grows with `nrow_max`.** → For `replace = TRUE` each
  draw stores `nrow_max` rows. `sample` shards hold resampled `Conc` rows (no fit
  blobs), so they are small relative to the `fit` blob layer; `1000L` is modest.
  The default is tunable per scenario for the rare large case.
- **Five specs touched (incl. archived ones).** → Larger surface than a one-knob
  tidy-up, but each delta is mechanical and the behaviour is preserved except the
  intended draw-size change; `openspec validate --strict` gates each.

## Migration Plan

1. Add `nrow_max` to `ssd_define_scenario()` (arg, `chk`, default, signature
   group, storage); move `nrow`-vs-draw-size validation to use it. Update
   `R/params.R` roxygen.
2. `R/task-lists.R`: drop `grid$n_max` from `sample_task_grid()`; drop the
   emitted `ci` column from `hc_grid_tbl()` (keep the scalar-`ci`
   canonicalisation); update the baseline runner to read `nrow_max`/`ci` from
   `scenario$…` and compute `D`.
3. `R/targets-runner.R`: extend `scenario_step_slice()` (sample → `nrow_max`,
   hc → `ci`); `ssd_run_sample_step()` computes `D` from `nrow_max` + dataset;
   `ssd_run_hc_step()` reads `ci` from the slice.
4. Update specs (the five deltas), `TARGETS-DESIGN.md` §5 prose + §12 (done at
   propose), `GLOSSARY.md` (`n_max` entry rework + new `nrow_max` entry).
5. Re-baseline snapshots; refresh `man/`; sweep example scripts/templates;
   `air format`; `devtools::test()` / `R CMD check`.

Rollback is a straight revert: no data migration; the primer/partition model is
unchanged.

## Open Questions

- **Default value of `nrow_max`.** `1000L` recommended; confirm (lower, e.g.
  `200L`, trims `replace = TRUE` `sample`-shard storage at the cost of a lower
  no-churn ceiling for `nrow`).
- **`replace = FALSE` over-spec: cap silently or validate?** This design caps to
  `min(nrow_max, nrow(data))` (silent full permutation). The alternative is to
  abort when `nrow_max > nrow(data)`. Capping keeps "one high default" working;
  confirm that is preferred over an explicit error.
- **Where to store `nrow_max` on the scenario** — top-level `scenario$nrow_max`
  vs. a `scenario$sample$nrow_max` (a `sample`-step home mirroring `scenario$hc`).
  Either is slice-able; pick for accessor consistency.
