# The design for the design targets example, sourced by `_targets.R`.
#
# A *design* unions several scenarios into one (possibly ragged) pipeline: it
# lets you explore finer detail over a *subset* of the axes without computing the
# full cross-product. Members sharing a `seed` share their coincident shards
# (computed once), and the run fans in one combined `summary.parquet` with a
# `scenario` identity column. Edit to taste.
library(ssdsims)

# shard on (dataset, sim) so each (dataset, sim) is its own cell
pb <- list(
  sample = c("dataset", "sim"),
  fit = c("dataset", "sim"),
  hc = c("dataset", "sim")
)

# A coarse grid over BOTH datasets (a broad, shallow sweep) ...
coarse <- ssd_define_scenario(
  ssd_scenario_data(
    boron = ssddata::ccme_boron,
    cadmium = ssddata::ccme_cadmium
  ),
  nsim = 5L,
  seed = 42L,
  nrow = c(5L, 10L, 20L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)

# ... plus a dense ZOOM into one region: only `boron`, more replicates, and a
# finer `nrow` sweep. The union is ragged - `boron` sims 1-5 are shared with
# `coarse` (built once, their `nrow` tasks merged), `cadmium` stays coarse-only,
# and `boron` sims 6-15 are the dense-only refinement. `boron` must be identical
# in both members (the consistency contract); `cadmium` appears only in `coarse`.
dense <- ssd_define_scenario(
  ssd_scenario_data(boron = ssddata::ccme_boron),
  nsim = 15L,
  seed = 42L,
  nrow = c(8L, 12L, 16L),
  dists = ssd_distset(lnorm = "lnorm"),
  partition_by = pb
)

# `ssd_design()` validates the collection (names must be unique and safe; a name
# shared across members must bind the same value). A design of one is valid too -
# the recommended starting point for a study that may grow (see the
# "From a Single Scenario to a Design" vignette).
design <- ssd_design(coarse = coarse, dense = dense)
