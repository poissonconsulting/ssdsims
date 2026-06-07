# Package index

## Scenarios and task expansion

Define a declarative simulation scenario and expand it into the per-step
task tables (`sample`, `fit`, `hc`) that the targets-based pipeline
builds on, with a baseline loop runner.

- [`ssd_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_data.md)
  : Assemble and Validate Datasets for a Simulation Scenario
- [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  : Define a Simulation Scenario
- [`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  : Expand a Scenario into all Three Task Tables
- [`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_tasks.md)
  : Derive the sample Task Table from a Scenario
- [`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_tasks.md)
  : Derive the fit Task Table from a Scenario
- [`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_tasks.md)
  : Derive the hc Task Table from a Scenario
- [`ssd_run_scenario_baseline()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_baseline.md)
  : Run a Scenario with the Baseline Loop Runner
- [`ssd_run_scenario_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_scenario_shards.md)
  : Run a Scenario over Hive-partitioned Parquet Shards (single core)

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

## Targets pipeline

Group a step’s tasks into per-shard tables keyed by `partition_by`, run
a shard with the per-task RNG primitives writing one Parquet per shard,
and fan in a summary - the building blocks of the static-branching
`targets` pipeline (see the `inst/targets-templates/small/_targets.R`
template).

- [`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_sample_shards.md)
  : Group sample Tasks into Shards
- [`ssd_scenario_fit_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_fit_shards.md)
  : Group fit Tasks into Shards
- [`ssd_scenario_hc_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_hc_shards.md)
  : Group hc Tasks into Shards
- [`ssd_run_sample_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_sample_step.md)
  : Run a sample Shard
- [`ssd_run_fit_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_fit_step.md)
  : Run a fit Shard
- [`ssd_run_hc_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_hc_step.md)
  : Run an hc Shard
- [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
  : Build the Targets Pipeline for a Scenario
- [`ssd_summarize()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarize.md)
  : Summarise a Run's hc Estimates Across Shards
- [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
  : Layout-keyed Results Root for a Scenario

## Manifest

A per-scenario JSON manifest: write the declarative head (scenario
fields plus complete session info for the bit-stability contract),
record each completed shard’s sha256 in a per-shard sidecar, and
assemble those into the manifest’s `completed_shards` map.

- [`ssd_write_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_write_manifest.md)
  : Write a Per-Scenario Manifest

- [`ssd_read_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_read_manifest.md)
  : Read a Per-Scenario Manifest

- [`ssd_record_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_record_shard.md)
  : Record a Completed Shard's sha256 Alongside its Parquet

- [`ssd_assemble_manifest()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_assemble_manifest.md)
  :

  Assemble `completed_shards` from the Shards on Disk

## Scenario accessors

A technical detail of the pipelines: isolate an already-materialised
value from a scenario by name - the dataset tibble or the `min_pmix`
function. Names (not values) drive task hashing, so these accessors
resolve a name back to the value carried on the scenario for execution.

- [`scenario_dataset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_dataset.md)
  : Isolate a Materialised Dataset from a Scenario by Name

- [`scenario_min_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_min_pmix.md)
  :

  Isolate a Materialised `min_pmix` Function from a Scenario by Name

## Reproducible RNG

Parallel-safe seeding helpers for the two RNG paths: the dqrng + hash
backend (targets path) and L’Ecuyer-CMRG sub-streams (legacy path).

- [`task_primer()`](https://poissonconsulting.github.io/ssdsims/reference/task_primer.md)
  : Derive a Per-task Primer from its Parameters
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
  : ssdsims: Simulation Analyses for Species Sensitivity Distributions
