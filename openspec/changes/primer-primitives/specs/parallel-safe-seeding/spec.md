## ADDED Requirements

### Requirement: Per-task seed-and-run wrappers install a primer exactly once
The package SHALL provide three internal per-task **seed-and-run** wrappers — `sample_data_task_primer()`, `fit_data_task_primer()`, and `hc_data_task_primer()` — on the dqrng + primer contract. The naming follows the corrected GLOSSARY convention: the per-task value is a **primer** (not a `state`), so the functions use the `_primer` suffix and a `primer` argument. Each takes a scalar `seed` and a length-2 integer `primer`, calls `local_dqrng_state(seed, primer)` **exactly once** to install the per-task RNG starting point, then invokes the matching state-less op (`sample_data_task()`, `fit_data_task()`, `hc_data_task()`) against the now-set ambient RNG. The state-less ops SHALL NOT take a `primer`/`state`/`stream` argument. A wrapper SHALL leave the surrounding RNG state unchanged beyond its `local_dqrng_state()` scope, and SHALL assume an already-active `local_dqrng_backend()`.

#### Scenario: sample wrapper seeds then draws
- **WHEN** `sample_data_task_primer(data, n_max, replace, seed, primer)` is called within an active dqrng backend
- **THEN** it SHALL install `(seed, primer)` once via `local_dqrng_state()` and return `sample_data_task(data, n_max, replace)` — i.e. `dplyr::slice_sample(data, n = n_max, replace = replace)` drawn from that state

#### Scenario: fit wrapper seeds then fits
- **WHEN** `fit_data_task_primer(data, <fit grid>, seed, primer)` is called
- **THEN** it SHALL install `(seed, primer)` once and return `fit_data_task(data, <fit grid>)`, whose inner `ssdtools::ssd_fit_dists()` call takes no `primer`/`state`/`stream` argument

#### Scenario: hc wrapper seeds then estimates
- **WHEN** `hc_data_task_primer(fits, <hc grid>, seed, primer)` is called
- **THEN** it SHALL install `(seed, primer)` once and return `hc_data_task(fits, <hc grid>)`, whose inner `ssdtools::ssd_hc()` call takes no `primer`/`state`/`stream` argument

#### Scenario: Seeding happens exactly once per wrapper
- **WHEN** any `*_data_task_primer()` wrapper runs
- **THEN** `local_dqrng_state()` SHALL be invoked exactly once, and the state-less op SHALL consume RNG only from that installed state

#### Scenario: State-less ops are RNG-agnostic
- **WHEN** a state-less op (`sample_data_task()`, `fit_data_task()`, `hc_data_task()`) is called directly
- **THEN** it SHALL perform its operation against the ambient RNG with no `seed`/`primer`/`state`/`stream` argument, leaving seeding entirely to the wrapper

### Requirement: Same (seed, primer) reproduces a wrapper's result
For a fixed `seed` and `primer`, a `*_data_task_primer()` wrapper SHALL produce an identical result on repeated calls, and a different `primer` (or `seed`) SHALL in general produce a different result. The wrappers take the primer as an argument (computed by the caller via `task_primer()`); they do not derive it themselves.

#### Scenario: Reproducible draw
- **WHEN** `sample_data_task_primer(data, n_max, replace, seed, primer)` is called twice with the same `(seed, primer)`
- **THEN** the two draws SHALL be identical

#### Scenario: Distinct primers diverge
- **WHEN** two calls share `seed` but differ in `primer`
- **THEN** their results SHALL in general differ (independent streams)

### Requirement: nrow sub-truncation under seeding
The `sample`-step draw SHALL be a single `sample_data_task_primer()` of `n_max = max(nrow)` rows keyed by the `(dataset, sim, replace)` primer; the `fit` step SHALL truncate it with `head(sample, nrow)` (RNG-free). A size-`n` truncation SHALL be a byte-identical prefix of the size-`n_max` draw, for both `replace = FALSE` and `replace = TRUE`.

#### Scenario: head(n) is a prefix of the n_max draw
- **WHEN** a `sample` draw of `n_max` rows is produced by `sample_data_task_primer()` and truncated to two sizes `n1 < n2 <= n_max`
- **THEN** `head(draw, n1)` SHALL be a byte-identical prefix of `head(draw, n2)`, so all `nrow` values share the one seeded draw

### Requirement: Baseline runner is reproducible per task
`ssd_run_scenario_baseline()` SHALL seed each `sample`/`fit`/`hc` task exactly once, via its `*_data_task_primer()` wrapper, with `seed = scenario$seed` and the per-task primer derived from the task's identity (`task_primer()`), under a single `local_dqrng_backend()` scope for the run. The runner's results SHALL be reproducible for a fixed `scenario$seed` **without** an externally pinned RNG, and SHALL be independent of the order in which tasks run.

#### Scenario: Re-running yields identical results
- **WHEN** `ssd_run_scenario_baseline(scenario)` is called twice for a scenario with a fixed `seed`, with no external `set.seed()`/`withr::with_seed()`
- **THEN** the two runs SHALL produce identical per-task `sample`/`fit`/`hc` results

#### Scenario: Results are order-independent
- **WHEN** the same task is run as part of the full scenario and again in isolation (same `scenario$seed` and task identity)
- **THEN** its result SHALL be identical, because its primer fully determines its RNG starting point

#### Scenario: Backend reset after the run
- **WHEN** `ssd_run_scenario_baseline()` returns (or errors)
- **THEN** the dqrng backend SHALL be reset and the caller's base RNG state SHALL be unchanged
