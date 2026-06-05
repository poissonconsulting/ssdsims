## MODIFIED Requirements

### Requirement: Reproducible fitting
Given the same input tibble and the same `seed`, the function SHALL produce identical `fits` output across invocations, seeding each per-task fit via the dqrng + per-task primer contract — `local_dqrng_state(seed, task_primer(identity))` under a scenario-scoped, reentrant `local_dqrng_backend()` — rather than the L'Ecuyer-CMRG sub-stream lattice, reusing the `fit_data_task_primer()` wrapper. The per-task primer SHALL be derived from the fit task's identity (its `sim`/`stream` together with its fit-grid row: `nrow`, `rescale`, `computable`, `at_boundary_ok`, the `min_pmix` **name**, `range_shape1`, `range_shape2`), so results are independent of the order in which fits run. `min_pmix` enters the primer by name, not by function value, so a recompile or JIT does not move a task's primer.

#### Scenario: Deterministic fits
- **WHEN** `ssd_fit_dists_sims()` is invoked twice with the same `x` and same `seed`
- **THEN** the resulting `fits` columns SHALL be identical

#### Scenario: Order-independent fits
- **WHEN** a fit task is computed standalone and again as one row of a larger fit grid, with the same `seed` and task identity
- **THEN** the two `fitdists` results SHALL be identical

### Requirement: Default fit configuration matches ssdtools
Default argument values SHALL mirror the `ssdtools::ssd_fit_dists()` defaults relevant to simulation use, specifically `dists = ssdtools::ssd_dists_bcanz()`, `rescale = FALSE`, `computable = FALSE`, `at_boundary_ok = TRUE`, `range_shape1 = list(c(0.05, 20))`, and `range_shape2 = range_shape1`. `min_pmix` SHALL default to the **name** `"ssd_min_pmix"` (a character vector), resolved to a function via `resolve_min_pmix()` (the `ssdtools` namespace, then the global environment), so the default fit uses `ssdtools::ssd_min_pmix`.

#### Scenario: Omitted arguments use defaults
- **WHEN** `ssd_fit_dists_sims()` is called with only `x`
- **THEN** the function SHALL fit distributions from `ssdtools::ssd_dists_bcanz()` without rescaling, allowing parameters at boundaries, with shape-parameter bounds of `[0.05, 20]`, resolving `min_pmix = "ssd_min_pmix"` to `ssdtools::ssd_min_pmix`

#### Scenario: min_pmix supplied by name
- **WHEN** `ssd_fit_dists_sims()` is called with `min_pmix = "ssd_min_pmix"` (or a vector of such names)
- **THEN** each name SHALL be resolved via `resolve_min_pmix()` and used as the mixture-proportion function for that fit configuration

### Requirement: Argument validation
The function SHALL validate each argument and abort with an informative error on invalid input.

#### Scenario: Missing data column
- **WHEN** the input tibble lacks a `data` column
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an error

#### Scenario: Unresolvable min_pmix name
- **WHEN** `min_pmix` is not a character vector, or names a function that `resolve_min_pmix()` cannot resolve in the `ssdtools` namespace or the global environment
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an informative error naming the offending value

#### Scenario: Invalid shape range length
- **WHEN** `range_shape1` contains a numeric vector of length other than 2
- **THEN** `ssd_fit_dists_sims()` SHALL abort with an error
