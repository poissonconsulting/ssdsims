# Test fixture: two members of a seed group that differ ONLY in a readout
# (`proportion`) - otherwise a coincident hc cell. The cell is one shared shard
# carrying the union of the proportions; each member's summary filters its slice.
# Used by test-design-targets.R.
library(targets)
library(tarchetypes)
library(ssdsims)

d <- readRDS("data.rds")
pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)

lo <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 1L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"),
  proportion = 0.05,
  partition_by = pb
)
hi <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 1L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"),
  proportion = 0.1,
  partition_by = pb
)

ssd_design_targets(ssd_design(lo = lo, hi = hi), root = "results")
