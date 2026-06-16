# Test fixture: the `ssd_scenario_targets()` factory over a saved numeric dataset
# (data.rds), partitioned per-sim through all three steps. The `fit`-only
# (`dists`) and `hc`-only (`samples`) scenario options are read from `opts.rds`
# so a test can flip one scenario option between `tar_make()` runs and assert
# (via `tar_outdated()`) that only the steps whose minimal scenario slice
# changed are rebuilt. Used by the step-scenario-slice tests in
# test-task-shards.R.
library(targets)
library(tarchetypes)
library(ssdsims)

opts <- readRDS("opts.rds")

scenario <- ssd_define_scenario(
  ssd_scenario_data(d = readRDS("data.rds")),
  nsim = 1L,
  seed = 42L,
  nrow = 6L,
  dists = opts$dists,
  samples = opts$samples,
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim")
  )
)

ssd_scenario_targets(scenario, root = "results")
