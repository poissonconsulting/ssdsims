# hazard-concentrations Delta

## MODIFIED Requirements

### Requirement: Factorial expansion over HC configurations
The function SHALL cross-join the input tibble with the cartesian product of the vector-valued bootstrap axes `nboot`, `ci_method`, and `parametric`, producing one row per (simulation × bootstrap-configuration) combination. `est_method` SHALL NOT be part of this factorial expansion: it is an hc scenario setting whose every requested value is summarised from each cell's single bootstrap sample set (see "est_method summaries derived from a single bootstrap sample set").

#### Scenario: Multiple CI methods
- **WHEN** `ssd_hc_sims()` is called with `ci_method = c("multi_fixed", "weighted_samples")` on an input of `N` rows
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, ci_method) pair

#### Scenario: Multiple est_methods do not multiply rows
- **WHEN** `ssd_hc_sims()` is called with `est_method = c("arithmetic", "multi")` (and otherwise scalar bootstrap axes) on an input of `N` rows
- **THEN** the returned tibble SHALL still have `N` rows (one per simulation × bootstrap-configuration), each carrying an `hc` tibble whose rows cover both `est_method` values
