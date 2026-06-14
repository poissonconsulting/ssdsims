## Why

With the fixed `nrow_max` draw (`nrow-max-setting`, same branch), sampling
**with** replacement is the natural default for a simulation study: the
bootstrap-style draw is the standard resampling model, and it frees `nrow` from
the dataset-size ceiling — under `replace = FALSE` the effective draw caps at
`nrow(data)`, so a default scenario on a small SSD dataset (e.g. 28-row
`ccme_boron`) cannot sweep `nrow` past the dataset size, which is exactly the
sweep a simulation study wants. `replace = FALSE` (a permutation draw) remains
available as an explicit choice.

Flipping the default also surfaces a behaviour worth pinning: **what happens
when `replace = c(FALSE, TRUE)` and some `nrow` exceeds a dataset's size?**
Under `replace = FALSE` the effective draw caps at `min(nrow_max, nrow(data))`,
so an `nrow` above that cap has no valid permutation draw for that dataset.
Rather than poisoning the whole scenario — aborting construction because one
cell of an otherwise-feasible cross-join cannot be drawn — the pipeline
**silently discards** those infeasible `(dataset, replace = FALSE, nrow)` tasks.
Every feasible cell still runs: all `replace = TRUE` cells, and the
`replace = FALSE` cells within each dataset's cap. The task set therefore stays
a deterministic function of the scenario (the datasets are part of it) — it is
simply the cross-join minus the cells that cannot be drawn, rather than a strict
rectangle. This change makes that discard an explicit spec scenario with
per-dataset tests, so the mixed-`replace` semantics are contractual, not
incidental.

## What Changes

- **`replace` defaults to `TRUE`** in `ssd_define_scenario()` (was `FALSE`).
  Nothing else about `replace` changes: it remains a structural cross-join axis
  (1–2 unique logical values), and an explicit `replace = FALSE` or
  `replace = c(FALSE, TRUE)` behaves as described below.
- **Infeasible `replace = FALSE` cells are silently discarded**: where `replace`
  includes `FALSE`, any `(dataset, nrow)` combination with
  `nrow > min(nrow_max, nrow(data))` is dropped from the `replace = FALSE`
  portion of the task grid (no warning, no short draw), while the matching
  `replace = TRUE` cells and all feasible `replace = FALSE` cells are untouched.
  The discard is per dataset, so on a multi-dataset scenario only the datasets
  too small for a given `nrow` lose their `replace = FALSE` cell at that `nrow`.
- **`replace = TRUE` infeasibility still aborts**: an `nrow` above `nrow_max`
  (the scenario's own draw ceiling, independent of any dataset) remains a
  construction error, as does a scenario whose grid is left **empty** after the
  `replace = FALSE` discard.
- **BREAKING (pre-1.0)**: every scenario built without an explicit `replace`
  now samples with replacement — the default draw is `nrow_max` rows (not the
  dataset permutation), so default-scenario draws, task ids
  (`replace=TRUE` path cells), prints, and downstream results re-baseline.
  Explicit-`replace` scenarios are unaffected.
- **Out of scope**: the legacy `ssd_sim_data()`/`ssd_run_scenario()` path keeps
  its `replace = FALSE` default — it is superseded by the scenario API and is
  being reworked wholesale in `migrate-public-api`.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `scenario-definition`: `replace` defaults to `TRUE`; infeasible
  `(dataset, replace = FALSE, nrow)` cells are silently discarded from the task
  grid rather than aborting the scenario, while `replace = TRUE` over-`nrow_max`
  and the all-empty grid still abort — captured as explicit requirement
  scenarios.

## Impact

- **Code**: `R/scenario.R` (default flip; relax the per-dataset
  `replace = FALSE` construction abort introduced by `nrow-max-setting` to the
  `replace = TRUE` / `nrow_max` and empty-grid cases), `R/task-lists.R` (filter
  infeasible `replace = FALSE` cells out of the `sample`/`fit` grids),
  `R/params.R` (`@param replace` notes the default).
- **Tests**: default-`replace` expectations flip (`unique(tasks$replace)`,
  `replace=TRUE` path ids); tests whose *point* is the permutation cap set
  `replace = FALSE` explicitly; new mixed-`replace` discard and per-dataset
  tests (task counts reflect dropped cells; empty-grid abort); snapshots
  re-record (`_snaps/scenario.md`, `_snaps/task-lists.md`).
- **Docs**: `man/` regenerated; `GLOSSARY.md` `replace` entry notes the
  default; vignettes re-render (prose already conditional on `replace`).
- **Dependencies**: builds on `nrow-max-setting` (same branch, PR #149) — the
  effective-draw-size definition this change filters on is implemented there. No
  dependants.
