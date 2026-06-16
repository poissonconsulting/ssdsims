# Test fixture: two members of a seed group that differ ONLY in their `distset`
# coverage (hence in their fit `dists` union). They share every sample/fit shard
# (one design-wide union fit of {lnorm, gamma}) and differ only in their `distset`
# hc cells; each member's hc subset equals its standalone run.
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

one <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 1L,
  seed = 42L,
  nrow = 10L,
  dists = ssd_distset(one = "lnorm"),
  partition_by = pb
)
both <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 1L,
  seed = 42L,
  nrow = 10L,
  dists = ssd_distset(one = "lnorm", two = c("lnorm", "gamma")),
  partition_by = pb
)

ssd_design_targets(ssd_design(one = one, both = both), root = "results")
