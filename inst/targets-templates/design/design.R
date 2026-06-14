# The design for the design targets example, sourced by `_targets.R`.
#
# A *design* unions several scenarios into one (possibly ragged) pipeline: it
# lets you explore finer detail over a *subset* of the axes without computing the
# full cross-product. Members sharing a `seed` share their coincident shards
# (computed once), and the run fans in one combined `summary.parquet` with a
# `scenario` identity column. Edit to taste.
library(ssdsims)

data <- ssd_scenario_data(ssddata::ccme_boron)

# A coarse grid everywhere ...
coarse <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = c(5L, 10L, 20L),
  dists = ssd_distset(lnorm = "lnorm")
)

# ... plus a dense refinement over just the extra `nrow` values. It shares the
# `seed`, dataset, and distributions with `coarse`, so it reuses every `sample`
# shard (the draw does not depend on `nrow`) and only its new `nrow` cells are
# extra work - the irregular (ragged) grid.
dense <- ssd_define_scenario(
  data,
  nsim = 2L,
  seed = 42L,
  nrow = c(12L, 14L, 16L, 18L),
  dists = ssd_distset(lnorm = "lnorm")
)

# `ssd_design()` validates the collection (names must be unique and safe; a name
# shared across members must bind the same value). A design of one is valid too -
# the recommended starting point for a study that may grow (see the
# "From a Single Scenario to a Design" vignette).
design <- ssd_design(coarse = coarse, dense = dense)
