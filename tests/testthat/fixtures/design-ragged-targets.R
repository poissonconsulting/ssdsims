# Test fixture: a genuinely ragged design - two members covering *different,
# overlapping* regions of the (dataset, sim) grid.
#
#   coarse: datasets {a, b} x sim {1, 2}              (a broad, shallow sweep)
#   dense : dataset  {a}    x sim {1, 2, 3, 4}        (zoom into `a`: more sims,
#           with a finer `nrow` sweep too)
#
# Cells (the partition_by path is (dataset, sim)):
#   shared        : (a,1) (a,2)   -> built ONCE, read by both members
#   coarse-only    : (b,1) (b,2)
#   dense-only      : (a,3) (a,4)
# so the union is 6 sample/fit/hc shards (not coarse's 4 + dense's 4 = 8). On the
# shared (a,1)/(a,2) cells the fit/hc tasks merge both members' `nrow` sweeps.
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

# `a` is identical in both members (the consistency contract); `b` only appears
# in `coarse`.
coarse <- ssd_define_scenario(
  ssd_scenario_data(a = d, b = d),
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)
dense <- ssd_define_scenario(
  ssd_scenario_data(a = d),
  nsim = 4L,
  seed = 42L,
  nrow = c(5L, 7L, 8L, 10L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)

design <- ssd_design(coarse = coarse, dense = dense)
ssd_design_targets(design, root = "results")
