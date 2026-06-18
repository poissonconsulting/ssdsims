# Test fixture: a design of two members identical except for `seed` (42 and 43).
# They share no draws, so they land under distinct `seed=` trees and every shard
# target is its own. Used by test-design-targets.R.
library(targets)
library(tarchetypes)
library(ssdsims)

data <- ssd_scenario_data(d = readRDS("data.rds"))
pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)
mk <- function(seed) {
  ssd_define_scenario(
    data,
    nsim = 1L,
    seed = seed,
    nrow = 6L,
    dists = ssd_distset(lnorm = "lnorm"),
    partition_by = pb
  )
}

design <- ssd_design(a = mk(42L), b = mk(43L))
ssd_design_targets(design, root = "results")
