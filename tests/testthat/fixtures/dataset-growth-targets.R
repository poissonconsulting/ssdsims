# Test fixture: the `ssd_scenario_targets()` factory over a *variable* set of
# saved numeric datasets (datasets.rds, a named list of data frames), partitioned
# per dataset/sim. A test runs `tar_make()`, appends a dataset to datasets.rds,
# and re-sources to assert (via `tar_outdated()`) that appending a dataset mints
# only the new dataset's shards and leaves every existing shard cached - the
# per-dataset `sample` slice is what makes the existing shards' commands
# byte-identical. Used by the step-scenario-slice path-axis-growth test in
# test-task-shards.R.
library(targets)
library(tarchetypes)
library(ssdsims)
library(dqrng)

datasets <- readRDS("datasets.rds")

scenario <- ssd_define_scenario(
  do.call(ssd_data, datasets),
  nsim = 1L,
  nrow = 6L,
  seed = 42L,
  dists = "lnorm",
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim")
  )
)

ssd_scenario_targets(scenario, root = "results")
