## MODIFIED Requirements

### Requirement: samples retains the bootstrap draws (output only)
`ssd_define_scenario()` SHALL accept a scalar logical `samples` argument (default `FALSE`), validated as a flag (a single non-`NA` `TRUE`/`FALSE`), stored at `scenario$hc$samples`, and passed to `ssdtools::ssd_hc()` so that `samples = TRUE` retains the per-row bootstrap draws in the hc `samples` list-column. `samples` SHALL NOT be a grid axis and SHALL NOT enter the task identity (`task_axes("hc")`) or the per-task RNG primer: it does not change the estimates, so changing it SHALL yield byte-identical `est`/`lcl`/`ucl` while re-running the hc step to populate (or empty) the `samples` column. `print.ssdsims_scenario()` SHALL render `samples` among the hc scenario options.

#### Scenario: samples defaults to FALSE and is stored
- **WHEN** `ssd_define_scenario()` is called without `samples`
- **THEN** `scenario$hc$samples` SHALL be `FALSE`

#### Scenario: samples = TRUE retains draws without changing estimates
- **WHEN** a scenario is run with `samples = TRUE` versus `FALSE` (same seed, `ci = TRUE`)
- **THEN** the hc estimates SHALL be byte-identical, and the `samples` list-column SHALL be populated only when `samples = TRUE`

#### Scenario: samples must be a flag
- **WHEN** `ssd_define_scenario(..., samples = c(TRUE, FALSE))` (or any non-flag) is called
- **THEN** the constructor SHALL abort in the user-facing frame
