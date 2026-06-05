## MODIFIED Requirements

### Requirement: Argument validation
The function SHALL validate arguments against the methods exposed by `ssdtools` and abort on invalid input. `ci` SHALL be validated as a flag (a single non-`NA` `TRUE`/`FALSE`); it is applied as a scalar and is not part of the function's factorial expansion.

#### Scenario: Unknown est_method
- **WHEN** `est_method` contains a value not in `ssdtools::ssd_est_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Unknown ci_method
- **WHEN** `ci_method` contains a value not in `ssdtools::ssd_ci_methods()`
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: Missing fits column
- **WHEN** the input tibble lacks a `fits` column
- **THEN** `ssd_hc_sims()` SHALL abort with an error

#### Scenario: ci must be a flag
- **WHEN** `ssd_hc_sims()` is called with a non-flag `ci` (e.g. `ci = c(FALSE, TRUE)`)
- **THEN** `ssd_hc_sims()` SHALL abort with an error, since `ci` is a scalar flag
