# Test fixture: the `ssd_scenario_targets()` factory over a *variable* set of
# distribution sets (distsets.rds, a named list of distribution-name vectors),
# with `distset` promoted to the hc path. A test runs `tar_make()`, appends a
# set whose members are a SUBSET of the existing fit union, and re-sources to
# assert (via `tar_outdated()`) that appending the set mints only the new hc
# shard and leaves every sample/fit shard and the pre-existing hc shard cached -
# the fit layer carries no `distset`, and the per-shard hc slice carries only the
# set(s) its shard reads. Used by the path-axis-growth test in
# test-path-axis-growth.R.
library(targets)
library(tarchetypes)
library(ssdsims)

distsets <- readRDS("distsets.rds")

scenario <- ssd_define_scenario(
  ssd_scenario_data(d = readRDS("data.rds")),
  nsim = 1L,
  seed = 42L,
  nrow = 6L,
  dists = do.call(ssd_distset, distsets),
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim", "distset")
  )
)

ssd_scenario_targets(scenario, root = "results")
