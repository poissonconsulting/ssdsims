# Test fixture: the motivating mix - a `ci = FALSE` member over more sims beside a
# `ci = TRUE` member over fewer. The overlapping sims bootstrap once (under
# `ci = TRUE`); the `ci = FALSE` member reads its (analytical, ci-invariant) `est`
# from those shards, and the non-overlapping sims stay `ci = FALSE`.
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

slow <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 4L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"),
  ci = FALSE,
  partition_by = pb
)
fast <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 2L,
  seed = 42L,
  nrow = 6L,
  dists = ssd_distset(lnorm = "lnorm"),
  ci = TRUE,
  nboot = 10L,
  partition_by = pb
)

ssd_design_targets(ssd_design(slow = slow, fast = fast), root = "results")
