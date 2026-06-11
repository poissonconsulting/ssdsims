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
when `replace = c(FALSE, TRUE)` and some `nrow` exceeds a dataset's size?** The
scenario grid is a rectangular cross-join, so every `(dataset, sim, replace)`
cell must support every `nrow` truncation. One infeasible cell — an `nrow`
above the `replace = FALSE` effective draw size (`min(nrow_max, nrow(data))`)
for *any* dataset — therefore poisons the whole scenario: construction aborts
(naming the offending dataset and draw size) rather than silently dropping
cells or producing a short draw. This is the already-implemented behaviour of
`nrow-max-setting`'s validation; this change makes it an explicit spec scenario
with per-dataset tests, so the mixed-`replace` semantics are contractual, not
incidental.

## What Changes

- **`replace` defaults to `TRUE`** in `ssd_define_scenario()` (was `FALSE`).
  Nothing else about `replace` changes: it remains a structural cross-join axis
  (1–2 unique logical values), and an explicit `replace = FALSE` or
  `replace = c(FALSE, TRUE)` behaves exactly as before.
- **Mixed-`replace` infeasibility is pinned**: with `replace` containing
  `FALSE`, each `nrow` is validated against `min(nrow_max, nrow(data))` for
  **every** dataset; with `replace` containing `TRUE`, against `nrow_max`. Any
  violation aborts construction in the user-facing frame — the cross-join is
  rectangular, so there is no per-cell dropout. The error names the offending
  dataset (for the `replace = FALSE` branch) so multi-dataset scenarios are
  debuggable.
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

- `scenario-definition`: `replace` defaults to `TRUE`; the mixed-`replace`
  rectangular-grid abort (any `nrow` infeasible under any included `replace`
  value, for any dataset, aborts construction naming the offender) becomes an
  explicit requirement scenario.

## Impact

- **Code**: `R/scenario.R` (one default flip; the validation already
  implements the pinned behaviour), `R/params.R` (`@param replace` notes the
  default).
- **Tests**: default-`replace` expectations flip (`unique(tasks$replace)`,
  `replace=TRUE` path ids); tests whose *point* is the permutation cap set
  `replace = FALSE` explicitly; new mixed-`replace` and per-dataset abort
  tests; snapshots re-record (`_snaps/scenario.md`, `_snaps/task-lists.md`).
- **Docs**: `man/` regenerated; `GLOSSARY.md` `replace` entry notes the
  default; vignettes re-render (prose already conditional on `replace`).
- **Dependencies**: builds on `nrow-max-setting` (same branch, PR #149) — the
  effective-draw-size validation this change pins is implemented there. No
  dependants.
