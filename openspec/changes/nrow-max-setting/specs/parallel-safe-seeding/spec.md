## MODIFIED Requirements

### Requirement: nrow sub-truncation under seeding
The `sample`-step draw SHALL be a single `sample_data_task_primer()` of the scenario's **effective draw size** `D` rows keyed by the `(dataset, sim, replace)` primer, where `D = min(nrow_max, nrow(data))` for `replace = FALSE` and `D = nrow_max` for `replace = TRUE` — `nrow_max` being the scenario's fixed draw-size setting, **not** `max(nrow)`. The `fit` step SHALL truncate the draw with `head(sample, nrow)` (RNG-free). A size-`n` truncation SHALL be a byte-identical prefix of the size-`D` draw, for both `replace = FALSE` and `replace = TRUE`. Because `D` does not depend on `max(nrow)`, adding an `nrow` value (within `D`) SHALL NOT change the draw.

#### Scenario: head(n) is a prefix of the effective draw
- **WHEN** a `sample` draw of `D` rows is produced by `sample_data_task_primer()` and truncated to two sizes `n1 < n2 <= D`
- **THEN** `head(draw, n1)` SHALL be a byte-identical prefix of `head(draw, n2)`, so all `nrow` values share the one seeded draw

#### Scenario: Adding an nrow value does not change the draw
- **WHEN** a scenario's `sample` draw is produced, then an additional `nrow` value (no greater than the effective draw size) is added to the scenario and the draw is produced again with the same `seed`
- **THEN** the two draws SHALL be byte-identical, because the draw size is the fixed `nrow_max`, not `max(nrow)`
