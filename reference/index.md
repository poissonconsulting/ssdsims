# Package index

## Scenarios and task expansion

Define a declarative simulation scenario and expand it into the per-step
task tables (`sample`, `fit`, `hc`) that the targets-based pipeline
builds on, with a baseline loop runner.

- [`ssd_scenario_data()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_data.md)
  : Assemble and Validate Datasets for a Simulation Scenario

- [`ssd_gen()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_gen.md)
  : Materialise Generator Datasets for a Simulation Scenario

- [`ssd_pmix()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_pmix.md)
  :

  Assemble and Validate `min_pmix` Functions for a Simulation Scenario

- [`ssd_distset()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_distset.md)
  : Assemble One or More Distribution Sets

- [`ssd_define_scenario()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_define_scenario.md)
  : Define a Simulation Scenario

- [`ssd_scenario_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  [`ssd_scenario_sample_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  [`ssd_scenario_fit_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  [`ssd_scenario_hc_tasks()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_tasks.md)
  : Expand a Scenario into Task Tables

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

- [`ssd_scenario_sample_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
  [`ssd_scenario_fit_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
  [`ssd_scenario_hc_shards()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_shards.md)
  : Group Tasks into Shards
- [`ssd_run_sample_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_step.md)
  [`ssd_run_fit_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_step.md)
  [`ssd_run_hc_step()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_run_step.md)
  : Run a Step Shard
- [`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md)
  : Build the Targets Pipeline for a Scenario
- [`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md)
  : Summarise a Run's hc Estimates Across Shards
- [`scenario_results_dir()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_results_dir.md)
  : Seed- and Layout-keyed Results Root for a Scenario

## Designs (combining scenarios)

Run several scenarios as one pipeline: a design is the de-duplicated
union of its members’ grids (the irregular/ragged grid - finer detail
over a subset of the axes without the full cross-product), addressed by
cell under a `seed=`/`layout=` tree. Build a
[`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md),
turn it into one targets pipeline with
[`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md),
and fan in per-scenario and combined summaries.

- [`ssd_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design.md)
  : Assemble and Validate a Design of Scenarios
- [`ssd_design_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_design_targets.md)
  : Build the Targets Pipeline for a Design
- [`ssd_summarise_member()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_member.md)
  : Summarise One Design Member from the Shared hc Shards
- [`ssd_summarise_design()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_design.md)
  : Combine Per-scenario Summaries into One Design Summary

## Cloud upload

Typed, self-validating upload destinations
([`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md),
[`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md))
and the class-dispatched generics that probe credentials, ship each
shard, and read the uploaded results back in place - the
remote-destination sibling of `root` on
[`ssd_scenario_targets()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_scenario_targets.md).

- [`ssd_upload_azure()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  [`ssd_upload_dryrun()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_azure.md)
  : Upload Destinations for a Scenario's Shards

- [`ssd_test_upload()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_test_upload.md)
  : Probe an Upload Destination's Credentials and Connectivity

- [`ssd_upload_shard()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_upload_shard.md)
  : Ship Shard (or Summary) Parquet Files to an Upload Destination

- [`ssd_open_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_open_uploaded.md)
  : Open Uploaded Results for Querying, In Place

- [`ssd_summarise_uploaded()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise_uploaded.md)
  :

  Summarise Uploaded Results, In Place (the cloud
  [`ssd_summarise()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_summarise.md))

## Cost estimation

Predict, before launching, roughly how much compute a scenario costs and
how long its single longest task runs. Calibrate the per-task cost model
on the target machine (or use the shipped default), then apply it to a
scenario read-only - no fit, bootstrap, or RNG.

- [`ssd_estimate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_estimate_cost.md)
  : Estimate a Scenario's Compute Cost and Longest Task
- [`ssd_calibrate_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost.md)
  : Calibrate the Per-task Cost Model on the Current Machine
- [`ssd_cost_calibration()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration.md)
  : Default Cost Calibration
- [`ssd_cost_calibration_default`](https://poissonconsulting.github.io/ssdsims/reference/ssd_cost_calibration_default.md)
  : Default Cost Calibration Object

## Cost analysis

Read a completed run’s observed compute back from the per-task timings
its fit/hc shards carry: attribute it to the scenario axes, compare it
against the prediction, and recalibrate the cost model from the measured
durations. All read-only - no pipeline, fit, bootstrap, or RNG.

- [`ssd_analyse_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_analyse_cost.md)
  : Analyse a Run's Observed Compute Cost
- [`ssd_compare_cost()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_compare_cost.md)
  : Compare Predicted Against Observed Compute Cost
- [`ssd_calibrate_cost_from_run()`](https://poissonconsulting.github.io/ssdsims/reference/ssd_calibrate_cost_from_run.md)
  : Recalibrate the Cost Model from an Observed Run

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

- [`scenario_distset()`](https://poissonconsulting.github.io/ssdsims/reference/scenario_distset.md)
  : Isolate a Distribution Set from a Scenario by Name

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
