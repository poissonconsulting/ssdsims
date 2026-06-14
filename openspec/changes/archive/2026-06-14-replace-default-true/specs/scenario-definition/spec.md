## ADDED Requirements

### Requirement: replace defaults to sampling with replacement
`ssd_define_scenario()` SHALL default `replace` to `TRUE`, so a scenario built
without an explicit `replace` samples with replacement and its shared draw is
`nrow_max` rows per `(dataset, sim, replace)` cell — decoupling the default
`nrow` sweep from each dataset's size. `replace` SHALL remain a structural
cross-join axis (one or two unique, non-missing logical values), and an
explicit `replace = FALSE` (the permutation draw, capped at
`min(nrow_max, nrow(data))`) SHALL behave as specified below.

Because a `replace = FALSE` permutation draw cannot exceed
`min(nrow_max, nrow(data))` for a given dataset, where `replace` includes
`FALSE` any `(dataset, nrow)` combination with
`nrow > min(nrow_max, nrow(data))` SHALL be silently discarded from the
`replace = FALSE` portion of the task grid — the constructor SHALL NOT abort, emit
a warning, or permit a short draw for it. The discard SHALL be per dataset, and
the corresponding `replace = TRUE` cells together with all feasible
`replace = FALSE` cells SHALL be unaffected. The task grid SHALL therefore be
the cross-join of the scenario's axes minus these infeasible `replace = FALSE`
cells, and SHALL remain a deterministic function of the scenario.

An `nrow` exceeding `nrow_max` itself (the scenario's own draw ceiling,
independent of any dataset) is out of the `[5, nrow_max]` range every `nrow`
must satisfy, so it SHALL abort construction in the user-facing frame
regardless of `replace` — with a message citing `nrow_max`'s value — rather than
being discarded (no draw of any `replace` can supply it). A scenario whose grid
is left empty by the `replace = FALSE` discard SHALL likewise abort.

#### Scenario: replace defaults to TRUE
- **WHEN** `ssd_define_scenario()` is called without `replace`
- **THEN** the scenario SHALL store `replace = TRUE`, the derived `sample` task
  table's `replace` values SHALL all be `TRUE`, and an `nrow` above a dataset's
  row count (but within `nrow_max`) SHALL be accepted

#### Scenario: mixed replace discards an nrow infeasible for the FALSE draw
- **WHEN** `ssd_define_scenario(..., nrow = 50L, replace = c(FALSE, TRUE))` is
  called with a dataset of fewer than 50 rows
- **THEN** the constructor SHALL succeed, the `replace = TRUE` cells at
  `nrow = 50L` SHALL be present in the task grid, and the `replace = FALSE`
  cells at `nrow = 50L` SHALL be absent (silently discarded, with no warning)

#### Scenario: the discard is per dataset
- **WHEN** `ssd_define_scenario()` is called with `replace` including `FALSE`,
  several datasets, and an `nrow` exceeding only the smallest dataset's
  effective draw size
- **THEN** only that dataset's `replace = FALSE` cell at that `nrow` SHALL be
  discarded; the other datasets SHALL retain their `replace = FALSE` cells at
  that `nrow`

#### Scenario: an all-infeasible FALSE-only grid aborts
- **WHEN** `ssd_define_scenario()` is called with `replace = FALSE` only and
  every `nrow` exceeds every dataset's effective draw size
- **THEN** the discard SHALL leave no feasible task and the constructor SHALL
  abort in the user-facing frame rather than return an empty scenario
