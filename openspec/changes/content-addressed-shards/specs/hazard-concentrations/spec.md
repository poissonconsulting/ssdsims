# hazard-concentrations Specification (delta)

## MODIFIED Requirements

### Requirement: est_method summaries derived from a single bootstrap sample set
For each (simulation × hc-configuration) cell, the bootstrap SHALL be run
**once** and the summary for **every** requested `est_method` derived from that
single sample set, rather than re-bootstrapping per method; the `hc` summary for a
cell SHALL contain one row per requested `est_method`. **At a fixed cell seed**,
each row's `est`/`se`/`lcl`/`ucl` SHALL be byte-identical to calling
`ssdtools::ssd_hc()` for that `est_method` alone with that **same seed** and
inputs (the point `est` is analytical and seed-independent; the CI is
`est_method`-invariant given a shared draw). To make this single sample set
reusable **across scenarios**, the bootstrap SHALL be materialisable as a
**content-addressed `draw` artifact** keyed on the bootstrap identity `(fit
identity, distset, nboot, ci_method, parametric)` — `distset` is part of the
identity because each model-averaging pool bootstraps its own members (the
`distset-hc-axis` hc axis), while `est_method` is **not** (the draw is invariant
to it) and `ci` only gates whether a draw exists — and the `est_method` summaries
SHALL be derived by an **RNG-free** step that reads that artifact. `est_method`
SHALL NOT appear in the bootstrap identity or factorial expansion; it is a
within-cell dimension applied at summary time.

#### Scenario: One bootstrap, all est_methods, same-seed identity
- **WHEN** one hc cell is computed at a fixed seed with `ci = TRUE` and
  `est_method = c("arithmetic", "geometric", "multi")`
- **THEN** the cell's hc summary SHALL contain one row per `est_method`, all
  derived from one shared bootstrap sample set, and each row SHALL equal the
  single-method `ssdtools::ssd_hc()` result seeded with that **same** cell seed
  (no per-method re-bootstrap)

#### Scenario: est_method does not multiply bootstrap work
- **WHEN** the hc step is computed on `N` input rows with `ci = TRUE` and `M`
  `est_method` values
- **THEN** the number of bootstrap passes SHALL be independent of `M` (one per
  `distset × nboot × ci_method × parametric` cell), while the summary still covers
  all `M` methods

#### Scenario: The bootstrap draw address is est_method-invariant
- **WHEN** the `draw` artifact's content address is computed for two hc summaries
  differing only in `est_method`, at the same bootstrap identity `(fit, distset,
  nboot, ci_method, parametric)`
- **THEN** the `draw` address SHALL be identical for both, so one `draw` is built
  and both RNG-free `summarise` steps read it (no second bootstrap) — within a
  scenario today, and across scenarios once a design layer composes these targets
