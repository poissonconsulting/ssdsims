# Test fixture: a ragged design - two members sharing a seed and dataset but
# covering different `nrow` (a fit inner axis): coarse {5,10} and dense {6,7,8}.
# They share every `sample` shard and the `fit`/`hc` shard cells (the union merges
# their per-`nrow` tasks), so the design computes the union of cells once. Used by
# test-design-targets.R.
library(targets)
library(tarchetypes)
library(ssdsims)

data <- ssd_scenario_data(d = readRDS("data.rds"))
pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)
coarse <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)
dense <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = c(6L, 7L, 8L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)

ssd_design_targets(ssd_design(coarse = coarse, dense = dense), root = "results")
