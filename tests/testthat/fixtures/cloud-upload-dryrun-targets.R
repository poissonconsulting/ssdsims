# Test fixture: the `ssd_scenario_targets()` factory over a single saved numeric
# dataset (data.rds) with `upload = ssd_upload_dryrun()`, so each step shard is
# paired with a no-op `upload_<step>` target that reaches no network. Used by the
# content-hash skip and no-op upload tests in test-upload.R: a re-driven
# `tar_make()` with unchanged shards re-runs no `upload_<step>` target.
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

ssd_scenario_targets(scenario, root = "results", upload = ssd_upload_dryrun())
