## Context

`nrow-max-setting` (same branch, PR #149) fixed the shared `sample` draw at the
scenario's `nrow_max` setting and validates each `nrow` against the effective
draw size — `min(nrow_max, nrow(data))` per dataset when `replace` includes
`FALSE`, `nrow_max` when it includes `TRUE`. With that in place, the
`replace = FALSE` default is the limiting choice: it caps the default
scenario's `nrow` sweep at the dataset size, while the with-replacement draw
(the standard resampling model for a simulation study) has no such ceiling.

## Goals / Non-Goals

**Goals:**

- Default `replace` to `TRUE` in `ssd_define_scenario()`.
- Pin the mixed-`replace` semantics as a contract: the grid is a rectangular
  cross-join, so an `nrow` infeasible under *any* included `replace` value for
  *any* dataset aborts construction — no cell dropout, no short draws.

**Non-Goals:**

- No change to `replace` as an axis (vocabulary, partitioning, primers).
- No change to the validation logic itself — `nrow-max-setting` implemented it;
  this change flips one default and adds the spec scenarios + tests.
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

### Decision: mixed `replace` keeps the rectangular-grid abort

With `replace = c(FALSE, TRUE)`, every `nrow` must satisfy **both** modes for
**every** dataset: `nrow <= nrow_max` (the `TRUE` draw) and
`nrow <= min(nrow_max, nrow(data))` per dataset (the `FALSE` draw). One
violation aborts construction in the user-facing frame, naming the offending
dataset and its effective draw size on the `FALSE` branch.

*Alternative considered — drop infeasible `(replace = FALSE, nrow)` cells.*
Rejected: the task graph is a pure cross-join of the scenario's axis values
(`task-lists`); a non-rectangular grid would make task counts, ids, and shard
sets depend on per-dataset row counts, breaking the "shard set is a pure
function of the scenario" model for marginal convenience. A user who wants
both modes with a large `nrow` should either drop `FALSE` or lower `nrow` —
the error says which constraint failed.

*Alternative considered — cap the infeasible truncation silently.* Rejected:
`head(draw, nrow)` returning fewer than `nrow` rows is a silently wrong
simulation cell.

## Risks / Trade-offs

- **Behavioural re-baseline for default scenarios.** Intended and pre-1.0;
  draws, `replace=TRUE` ids, prints, and snapshots re-record. Explicit
  `replace` is untouched.
- **Default draws grow** (`nrow_max = 1000L` rows with replacement vs the old
  `max(nrow)` permutation prefix). Accepted in `nrow-max-setting`'s storage
  analysis; tunable per scenario.

## Migration Plan

1. Flip the signature default; update `@param replace` and the GLOSSARY entry.
2. Re-point default-`replace` tests (`TRUE` expectations, `replace=TRUE` ids);
   make permutation-cap tests explicit `replace = FALSE`; add mixed-`replace`
   and per-dataset abort tests; re-record snapshots.
3. `devtools::document()`; render vignettes; `air format`; full test run;
   `openspec validate --strict`.

Rollback is a one-line revert of the default plus the re-recorded snapshots.

## Open Questions

(none)
