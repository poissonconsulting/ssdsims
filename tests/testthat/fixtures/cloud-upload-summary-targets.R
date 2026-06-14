# Test fixture: the `ssd_scenario_targets()` factory over a single saved numeric
# dataset (data.rds) with `upload = ssd_upload_dryrun()` and the bootstrap-draw
# retention switched by samples.rds, so the `summary` fan-in is paired with a
# no-op `upload_summary` target shipping the compact summary - and, when
# `samples` is TRUE, the full summary-samples.parquet alongside it. Used by the
# summary-upload end-to-end test in test-upload.R.
library(targets)
library(tarchetypes)
library(ssdsims)

scenario <- ssd_define_scenario(
  ssd_scenario_data(d = readRDS("data.rds")),
  nsim = readRDS("nsim.rds"),
  seed = 42L,
  nrow = 6L,
  dists = "lnorm",
  samples = readRDS("samples.rds"),
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim")
  )
)

ssd_scenario_targets(scenario, root = "results", upload = ssd_upload_dryrun())
