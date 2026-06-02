## ADDED Requirements

### Requirement: Per-task state primitives install a primer exactly once
The package SHALL provide three internal per-task primitives — `slice_sample_state()`, `fit_dists_state()`, and `hc_state()` — each on the dqrng + primer contract: the primitive takes a scalar `seed` and a length-2 integer `state` (the per-task primer), calls `local_dqrng_state(seed, state)` **exactly once** to install the per-task RNG starting point, then invokes the state-less ssdtools/dplyr operation against the now-set ambient RNG. The inner operation SHALL NOT take a `state`/`stream` argument. The primitive SHALL NOT advance or leave the surrounding RNG state changed beyond the `local_dqrng_state()` scope.

#### Scenario: slice_sample_state seeds then draws
- **WHEN** `slice_sample_state(data, n_max, seed, state, replace)` is called within an active dqrng backend
- **THEN** it SHALL install `(seed, state)` once via `local_dqrng_state()` and return `dplyr::slice_sample(data, n = n_max, replace = replace)` drawn from that state

#### Scenario: fit_dists_state seeds then fits
- **WHEN** `fit_dists_state(data, seed, state, ...)` is called with the fit-grid arguments
- **THEN** it SHALL install `(seed, state)` once and return `ssdtools::ssd_fit_dists(data, ...)` with no `state`/`stream` argument on the inner call

#### Scenario: hc_state seeds then estimates
- **WHEN** `hc_state(fits, seed, state, ...)` is called with the hc-grid arguments
- **THEN** it SHALL install `(seed, state)` once and return `ssdtools::ssd_hc(fits, ...)` with no `state`/`stream` argument on the inner call

#### Scenario: Seeding happens exactly once per primitive
- **WHEN** any `_state` primitive runs
- **THEN** `local_dqrng_state()` SHALL be invoked exactly once, and the inner operation SHALL consume RNG only from that installed state

### Requirement: Same (seed, primer) reproduces a primitive's result
For a fixed `seed` and `state`, a `_state` primitive SHALL produce an identical result on repeated calls, and a different `state` (or `seed`) SHALL in general produce a different result.

#### Scenario: Reproducible draw
- **WHEN** `slice_sample_state(data, n_max, seed, state, replace)` is called twice with the same `(seed, state)`
- **THEN** the two draws SHALL be identical

#### Scenario: Distinct primers diverge
- **WHEN** two calls share `seed` but differ in `state`
- **THEN** their results SHALL in general differ (independent streams)

### Requirement: nrow sub-truncation under seeding
The `sample`-step draw SHALL be a single `slice_sample_state()` of `n_max = max(nrow)` rows keyed by the `(dataset, sim, replace)` primer; the `fit` step SHALL truncate it with `head(sample, nrow)` (RNG-free). A size-`n` truncation SHALL be a byte-identical prefix of the size-`n_max` draw, for both `replace = FALSE` and `replace = TRUE`.

#### Scenario: head(n) is a prefix of the n_max draw
- **WHEN** a `sample` draw of `n_max` rows is produced by `slice_sample_state()` and truncated to two sizes `n1 < n2 <= n_max`
- **THEN** `head(draw, n1)` SHALL be a byte-identical prefix of `head(draw, n2)`, so all `nrow` values share the one seeded draw

### Requirement: Baseline runner is reproducible per task
`ssd_run_scenario_baseline()` SHALL seed each `sample`/`fit`/`hc` task exactly once, via its `_state` primitive, with `seed = scenario$seed` and the per-task primer derived from the task's identity (`task_primer()`), under a `local_dqrng_backend()` scope. The runner's results SHALL be reproducible for a fixed `scenario$seed` **without** an externally pinned RNG, and SHALL be independent of the order in which tasks run.

#### Scenario: Re-running yields identical results
- **WHEN** `ssd_run_scenario_baseline(scenario)` is called twice for a scenario with a fixed `seed`, with no external `set.seed()`/`withr::with_seed()`
- **THEN** the two runs SHALL produce identical per-task `sample`/`fit`/`hc` results

#### Scenario: Results are order-independent
- **WHEN** the same task is run as part of the full scenario and again in isolation (same `scenario$seed` and task identity)
- **THEN** its result SHALL be identical, because its primer fully determines its RNG starting point

#### Scenario: Backend reset after the run
- **WHEN** `ssd_run_scenario_baseline()` returns (or errors)
- **THEN** the dqrng backend SHALL be reset and the caller's base RNG state SHALL be unchanged
