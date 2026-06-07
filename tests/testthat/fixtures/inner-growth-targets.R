# Test fixture: the `ssd_scenario_targets()` factory over a saved numeric
# dataset (data.rds), partitioned per-sim. The `fit` inner axis `min_pmix`
# (not a `partition_by` axis) grows from one value to two when `grow.rds` is
# TRUE. Re-sourced across two `tar_make()` runs (grow FALSE then TRUE) by the
# inner-axis-growth test in test-task-shards.R, which asserts the affected
# `fit` shards are atomically rewritten byte-stably while the `sample` shards
# stay cached (TARGETS-DESIGN.md section 8.2 / shard-atomic-rewrite).
library(targets)
library(tarchetypes)
library(ssdsims)
library(dqrng)

loose <- function(n) 0.05
strict <- function(n) 0.1

# A `min_pmix` axis value adds one fit task to every fit shard; the `loose`
# task's axes (and so its `(seed, primer)` and `fit_id`) are identical in both
# runs, so its row must re-emit byte-identical after the grow.
min_pmix <- if (isTRUE(readRDS("grow.rds"))) {
  list(loose = loose, strict = strict)
} else {
  list(loose = loose)
}

scenario <- ssd_define_scenario(
  ssd_data(d = readRDS("data.rds")),
  nsim = 2L,
  seed = 42L,
  nrow = 6L,
  min_pmix = min_pmix,
  dists = "lnorm",
  partition_by = list(
    sample = c("dataset", "sim"),
    fit = c("dataset", "sim"),
    hc = c("dataset", "sim")
  )
)

ssd_scenario_targets(scenario, root = "results")
