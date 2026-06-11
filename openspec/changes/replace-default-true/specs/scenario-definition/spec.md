## ADDED Requirements

### Requirement: replace defaults to sampling with replacement
`ssd_define_scenario()` SHALL default `replace` to `TRUE`, so a scenario built
without an explicit `replace` samples with replacement and its shared draw is
`nrow_max` rows per `(dataset, sim, replace)` cell — decoupling the default
`nrow` sweep from each dataset's size. `replace` SHALL remain a structural
cross-join axis (one or two unique, non-missing logical values), and an
explicit `replace = FALSE` (the permutation draw, capped at
`min(nrow_max, nrow(data))`) SHALL behave unchanged.

Because the task grid is a rectangular cross-join, every `nrow` SHALL be
feasible under **every** included `replace` value for **every** dataset:
`nrow <= nrow_max` where `replace` includes `TRUE`, and
`nrow <= min(nrow_max, nrow(data))` per dataset where `replace` includes
`FALSE`. Any violation SHALL abort construction in the user-facing frame — the
constructor SHALL NOT drop infeasible cells or permit a short draw — and the
`replace = FALSE` branch's error SHALL name the offending dataset and its
effective draw size, so a multi-dataset scenario identifies which dataset is
too small.

#### Scenario: replace defaults to TRUE
- **WHEN** `ssd_define_scenario()` is called without `replace`
- **THEN** the scenario SHALL store `replace = TRUE`, the derived `sample` task
  table's `replace` values SHALL all be `TRUE`, and an `nrow` above a dataset's
  row count (but within `nrow_max`) SHALL be accepted

#### Scenario: mixed replace aborts on an nrow infeasible for the FALSE draw
- **WHEN** `ssd_define_scenario(..., nrow = 50L, replace = c(FALSE, TRUE))` is
  called with a dataset of fewer than 50 rows
- **THEN** the constructor SHALL abort in the user-facing frame (the
  `replace = FALSE` cell cannot support the truncation), even though the
  `replace = TRUE` cell could — no infeasible cell is silently dropped

#### Scenario: the abort names the offending dataset
- **WHEN** `ssd_define_scenario()` is called with `replace` including `FALSE`,
  several datasets, and an `nrow` exceeding only the smallest dataset's
  effective draw size
- **THEN** the constructor SHALL abort with an error naming that dataset and
  its effective draw size
