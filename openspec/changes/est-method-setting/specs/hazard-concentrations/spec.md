## ADDED Requirements

### Requirement: est_method summaries derived from a single bootstrap sample set
For each (simulation × hc-configuration) cell, `ssd_hc_sims()` SHALL run the
bootstrap **once** and derive the summary for **every** requested `est_method`
from that single retained sample set, rather than re-bootstrapping per method.
The `hc` tibble for a cell SHALL contain one row per requested `est_method`, and
each such row's `est`/`se`/`lcl`/`ucl` SHALL be **byte-identical** to the result
of calling `ssdtools::ssd_hc()` for that `est_method` alone with the same seed
and inputs. `est_method` SHALL therefore not appear in the function's factorial
expansion; it is a within-cell dimension.

#### Scenario: One bootstrap, all est_methods, identical estimates
- **WHEN** `ssd_hc_sims()` is called with `ci = TRUE`, a fixed `seed`, and `est_method = c("arithmetic", "geometric", "multi")`
- **THEN** each cell's `hc` tibble SHALL contain one row per `est_method`, and those rows' estimates SHALL equal the corresponding single-method `ssdtools::ssd_hc()` results, having been derived from one shared bootstrap sample set (no per-method re-bootstrap)

#### Scenario: est_method does not multiply bootstrap work
- **WHEN** `ssd_hc_sims()` is called on `N` input rows with `ci = TRUE` and `M` `est_method` values
- **THEN** the number of bootstrap passes SHALL be independent of `M` (one per `nboot × ci_method × parametric` cell, not `M` times that), while the returned `hc` rows still cover all `M` methods

## MODIFIED Requirements

### Requirement: Factorial expansion over HC configurations
The function SHALL cross-join the input tibble with the cartesian product of the vector-valued bootstrap axes `nboot`, `ci_method`, and `parametric`, producing one row per (simulation × bootstrap-configuration) combination. `est_method` SHALL NOT be part of this factorial expansion: it is an hc simulation setting whose every requested value is summarised from each cell's single bootstrap sample set (see "est_method summaries derived from a single bootstrap sample set").

#### Scenario: Multiple CI methods
- **WHEN** `ssd_hc_sims()` is called with `ci_method = c("multi_fixed", "weighted_samples")` on an input of `N` rows
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, ci_method) pair

#### Scenario: Multiple est_methods do not multiply rows
- **WHEN** `ssd_hc_sims()` is called with `est_method = c("arithmetic", "multi")` (and otherwise scalar bootstrap axes) on an input of `N` rows
- **THEN** the returned tibble SHALL still have `N` rows (one per simulation × bootstrap-configuration), each carrying an `hc` tibble whose rows cover both `est_method` values
