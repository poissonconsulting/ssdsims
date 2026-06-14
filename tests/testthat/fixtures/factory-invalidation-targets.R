# Test fixture: the `ssd_scenario_targets()` factory over a saved numeric dataset
# (data.rds), partitioned per-sim through all three steps so each shard subtree
# is sample_step_d_<sim> -> fit_step_d_<sim> -> hc_step_d_<sim>. Used by the
# hive-partitioning invalidation tests in test-task-shards.R to assert that
# invalidating one shard re-runs only its subtree.
library(targets)
library(tarchetypes)
library(ssdsims)

scenario <- ssd_define_scenario(
  ssd_scenario_data(d = readRDS("data.rds")),
  nsim = 2L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim")
  )
)

ssd_scenario_targets(scenario, root = "results")
