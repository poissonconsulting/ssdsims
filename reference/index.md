# Package index

## Scenarios and task expansion

Define a declarative simulation scenario and expand it into the per-step
task tables (`sample`, `data`, `fit`, `hc`) that the targets-based
pipeline builds on, with a baseline loop runner.

- [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
  : Assemble and Validate Datasets for a Simulation Scenario
- [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  : Define a Simulation Scenario
- [`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  : Expand a Scenario into all Four Task Tables
- [`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_tasks.md)
  : Derive the sample Task Table from a Scenario
- [`ssd_scenario_data_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data_tasks.md)
  : Derive the data Task Table from a Scenario
- [`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_tasks.md)
  : Derive the fit Task Table from a Scenario
- [`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_tasks.md)
  : Derive the hc Task Table from a Scenario
- [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
  : Run a Scenario with the Baseline Loop Runner

## Simulation pipeline

The immediate (non-targets) pipeline: simulate data, fit distributions,
and estimate hazard concentrations across simulations, end to end.

- [`ssd_sim_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_sim_data.md)
  : Generate Data for Simulations
- [`ssd_fit_dists_sims()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_fit_dists_sims.md)
  : Fit SSD Distributions to Simulated Data
- [`ssd_hc_sims()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_hc_sims.md)
  : Estimate hazard concentrations for multiple simulations using
  bootstrapping
- [`ssd_run_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario.md)
  : Run Scenario

## Reproducible RNG

Parallel-safe seeding helpers for the two RNG paths: the dqrng + hash
backend (targets path) and L’Ecuyer-CMRG sub-streams (legacy path).

- [`local_dqrng_backend()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_backend.md)
  : Local dqrng pcg64 Backend
- [`local_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  [`with_dqrng_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_dqrng_state.md)
  : Local/With dqrng State
- [`local_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
  [`with_lecuyer_cmrg_seed()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_seed.md)
  : Local/With L'Ecuyer-CMRG Seed
- [`local_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md)
  [`with_lecuyer_cmrg_state()`](https://poissonconsulting.github.io/ssdsims/reference/local_lecuyer_cmrg_state.md)
  : Local/With L'Ecuyer-CMRG State

## Package

- [`ssdsims`](https://poissonconsulting.github.io/ssdsims/reference/ssdsims-package.md)
  [`ssdsims-package`](https://poissonconsulting.github.io/ssdsims/reference/ssdsims-package.md)
  : ssdsims: What the Package Does (One Line, Title Case)
