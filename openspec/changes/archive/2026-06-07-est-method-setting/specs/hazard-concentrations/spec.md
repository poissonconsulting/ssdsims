## ADDED Requirements

### Requirement: est_method summaries derived from a single bootstrap sample set
For each (simulation × hc-configuration) cell, `ssd_hc_sims()` SHALL run the
bootstrap **once** and derive the summary for **every** requested `est_method`
from that single retained sample set, rather than re-bootstrapping per method.
The `hc` tibble for a cell SHALL contain one row per requested `est_method`.
**At a fixed cell seed**, each row's `est`/`se`/`lcl`/`ucl` SHALL be
byte-identical to calling `ssdtools::ssd_hc()` for that `est_method` alone with
that **same seed** and inputs (the point `est` is analytical and
seed-independent; the CI is est_method-invariant given a shared draw — see
`exploration/est-method-invariance.R`). This is a same-seed invariant, NOT a
claim of equality with the pre-change pipeline: because the hc primer hashes the
hc-grid row including `est_method` today, removing the axis re-seeds each hc task,
so collapsed-pipeline CIs differ numerically from the old per-`est_method`-seeded
output (point estimates are unchanged). `est_method` SHALL NOT appear in the
function's factorial expansion; it is a within-cell dimension.

#### Scenario: One bootstrap, all est_methods, same-seed identity
- **WHEN** `ssd_hc_sims()` computes one hc cell at a fixed seed with `ci = TRUE` and `est_method = c("arithmetic", "geometric", "multi")`
- **THEN** the cell's `hc` tibble SHALL contain one row per `est_method`, all derived from one shared bootstrap sample set, and each row SHALL equal the single-method `ssdtools::ssd_hc()` result seeded with that **same** cell seed (no per-method re-bootstrap)

#### Scenario: est_method does not multiply bootstrap work
- **WHEN** `ssd_hc_sims()` is called on `N` input rows with `ci = TRUE` and `M` `est_method` values
- **THEN** the number of bootstrap passes SHALL be independent of `M` (one per `nboot × ci_method × parametric` cell, not `M` times that), while the returned `hc` rows still cover all `M` methods

## MODIFIED Requirements

### Requirement: Factorial expansion over HC configurations
The function SHALL cross-join the input tibble with the cartesian product of the vector-valued bootstrap axes `nboot`, `ci_method`, and `parametric`, producing one row per (simulation × bootstrap-configuration) combination. `est_method` SHALL NOT be part of this factorial expansion: it is an hc scenario setting whose every requested value is summarised from each cell's single bootstrap sample set (see "est_method summaries derived from a single bootstrap sample set").

#### Scenario: Multiple CI methods
- **WHEN** `ssd_hc_sims()` is called with `ci_method = c("multi_fixed", "weighted_samples")` on an input of `N` rows
- **THEN** the returned tibble SHALL have `2 * N` rows, one per (simulation, ci_method) pair

#### Scenario: Multiple est_methods do not multiply rows
- **WHEN** `ssd_hc_sims()` is called with `est_method = c("arithmetic", "multi")` (and otherwise scalar bootstrap axes) on an input of `N` rows
- **THEN** the returned tibble SHALL still have `N` rows (one per simulation × bootstrap-configuration), each carrying an `hc` tibble whose rows cover both `est_method` values
