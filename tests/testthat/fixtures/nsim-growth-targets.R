# Test fixture: the `ssd_scenario_targets()` factory over a single saved numeric
# dataset (data.rds), with `nsim` read from nsim.rds so a test can grow `nsim`
# between `tar_make()` runs and assert (via `tar_progress()`) that growing `nsim`
# mints only the added `sim` values' shards and leaves every prior `sim`'s shard
# cached - `nsim` is in no step's scenario slice, so the existing shards' commands
# stay byte-identical. Used by the path-axis-growth test in
# test-path-axis-growth.R.
library(targets)
library(tarchetypes)
library(ssdsims)

scenario <- ssd_define_scenario(
  ssd_scenario_data(d = readRDS("data.rds")),
  nsim = readRDS("nsim.rds"),
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
