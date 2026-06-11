## Context

`nrow-max-setting` (same branch, PR #149) fixed the shared `sample` draw at the
scenario's `nrow_max` setting and defines the effective draw size —
`min(nrow_max, nrow(data))` per dataset when `replace` includes `FALSE`,
`nrow_max` when it includes `TRUE`. With that in place, the `replace = FALSE`
default is the limiting choice: it caps the default scenario's `nrow` sweep at
the dataset size, while the with-replacement draw (the standard resampling model
for a simulation study) has no such ceiling.

## Goals / Non-Goals

**Goals:**

- Default `replace` to `TRUE` in `ssd_define_scenario()`.
- Pin the mixed-`replace` semantics as a contract: an `nrow` infeasible for the
  `replace = FALSE` permutation draw of a given dataset is **silently
  discarded** from that dataset's `replace = FALSE` cells — the rest of the grid
  (every `replace = TRUE` cell, every feasible `replace = FALSE` cell) runs
  unchanged.

**Non-Goals:**

- No change to `replace` as an axis (vocabulary, partitioning, primers) beyond
  the per-dataset feasibility filter.
- No change to the effective-draw-size definition itself — `nrow-max-setting`
  defines it; this change consumes it to decide which cells are feasible.
- No change to the legacy `ssd_sim_data()`/`ssd_run_scenario()` defaults
  (superseded; reworked in `migrate-public-api`).

## Decisions

### Decision: default `replace = TRUE`

The default scenario draws `nrow_max` rows with replacement per
`(dataset, sim, replace)` cell. Under the `1000L` default that is a 1000-row
resample — modest storage (the `sample` layer holds `Conc` rows, no fit
blobs), and the `fit` step still operates on `head(draw, nrow)`.

*Alternative considered — keep `FALSE`.* Rejected: it silently couples the
default `nrow` ceiling to each dataset's size, which is exactly the limitation
`nrow_max` was introduced to remove.

### Decision: discard infeasible `replace = FALSE` cells rather than abort

With `replace = c(FALSE, TRUE)` (or the default `TRUE` widened to include
`FALSE`) over an `nrow` sweep, a large `nrow` is feasible for the with-
replacement draw but not for the permutation draw of a small dataset. The
constructor materialises the task grid as a cross-join, then **drops** the
`(dataset, nrow)` cells where `replace == FALSE` and
`nrow > min(nrow_max, nrow(data))`. The drop is per dataset and silent (no
warning, no short draw): the feasible cells — all `replace = TRUE` cells and the
in-cap `replace = FALSE` cells — are unaffected.

The task set is therefore the cross-join *minus the infeasible
`replace = FALSE` cells*. It is still a deterministic function of the scenario
(the datasets, and hence their row counts, are part of the scenario), and task
ids stay stable — they are derived from the `(dataset, sim, replace, nrow)`
tuple, and discarding a cell simply omits its tuple. Downstream code must
compute task/shard counts from the materialised grid rather than assuming a
strict `D × nsim × R × |nrow|` rectangle.

*Alternative considered — abort the whole scenario.* Rejected (this reverses the
change's earlier draft): a rectangular-grid abort lets one infeasible
permutation cell poison an otherwise-fine scenario, forcing the user to drop
`FALSE` or lower `nrow` even when most cells are runnable. With `replace = TRUE`
now the default, mixed-`replace` sweeps where the with-replacement run wants the
full `nrow` range and the permutation run wants only its feasible prefix are a
common, reasonable request; "run `replace = FALSE` where it is feasible" is the
least-surprising semantics.

*Alternative considered — cap the infeasible truncation silently (short draw).*
Rejected: `head(draw, nrow)` returning fewer than `nrow` rows is a silently
wrong simulation cell. Discarding the *task* is correct; shortening its *data*
is not.

*Alternative considered — emit a message naming the discarded cells.* Rejected
for now: the directive is to discard silently, and `replace = FALSE` infeasible
for a sweep is the expected, not exceptional, case. (A future opt-in summary
could list discarded cells without changing the default.)

### Decision: `replace = TRUE` over-`nrow_max` and an empty grid still abort

Two cases remain hard errors in the user-facing frame, because neither is a
per-dataset permutation-feasibility question:

- `nrow > nrow_max` while `replace` includes `TRUE` — `nrow_max` is the
  scenario's own draw ceiling, independent of any dataset, so exceeding it is a
  misconfiguration, not an infeasible cell.
- The `replace = FALSE` discard leaves **no** feasible task at all (e.g.
  `replace = FALSE` only, every `nrow` above every dataset's cap). An empty
  pipeline is a user error, so the constructor aborts rather than returning a
  scenario that produces nothing.

### Decision: filter in task expansion, not the constructor's stored axes

The scenario stores its axes (`datasets`, `nrow`, `replace`) verbatim; the
feasibility filter lives where the grid is materialised (`R/task-lists.R`,
`sample_task_grid` / `fit_task_table`). The `fit` grid drops rows where
`replace == FALSE` and `nrow` exceeds the dataset's effective draw size; a
`(dataset, replace = FALSE)` `sample` cell is dropped only when *no* `nrow`
survives for it. This keeps the stored scenario a faithful record of what the
user asked for and confines the non-rectangular logic to one place.

## Risks / Trade-offs

- **The task/shard set is no longer a plain rectangle.** It now depends on
  per-dataset row counts. Mitigation: it remains deterministic given the
  scenario (datasets are part of it) and task ids are unchanged for surviving
  cells; only count-derivation code that assumed rectangularity must read the
  materialised grid.
- **Silent discard can surprise.** A user may expect `replace = FALSE` results
  for an `nrow` that was quietly dropped. Accepted per the directive; the
  empty-grid guard catches the degenerate all-dropped case, and the
  per-dataset semantics are documented and tested.
- **Behavioural re-baseline for default scenarios.** Intended and pre-1.0;
  draws, `replace=TRUE` ids, prints, and snapshots re-record. Explicit
  `replace` is otherwise untouched.

## Migration Plan

1. Flip the signature default; update `@param replace` and the GLOSSARY entry.
2. Move per-dataset `replace = FALSE` feasibility from a construction abort
   (introduced by `nrow-max-setting`) to a silent discard in `R/task-lists.R`;
   keep the `replace = TRUE` / `nrow_max` abort and add the empty-grid guard.
3. Re-point default-`replace` tests (`TRUE` expectations, `replace=TRUE` ids);
   make permutation-cap tests explicit `replace = FALSE`; add mixed-`replace`
   discard, per-dataset, and empty-grid tests; re-record snapshots.
4. `devtools::document()`; render vignettes; `air format`; full test run;
   `openspec validate --strict`.

Rollback is a one-line revert of the default plus restoring the construction
abort and the re-recorded snapshots.

## Open Questions

(none)
