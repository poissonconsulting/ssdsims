# Test fixture: a pipeline whose `min_pmix` resolves to a function that errors,
# so every fit shard fails as a WHOLE shard under `error = "null"` while the
# sample shards still build. Used by the error-survival test in
# test-task-shards.R. Reads the saved numeric dataset (data.rds).
library(targets)
library(tarchetypes)
library(ssdsims)

boom <- function(n) stop("boom")

scenario <- ssd_define_scenario(
  ssd_data(d = readRDS("data.rds")),
  nsim = 2L,
  seed = 42L,
  nrow = 6L,
  min_pmix = boom,
  dists = "lnorm"
)

sample_targets <- tar_map(
  values = ssd_scenario_sample_shards(scenario),
  names = c(dataset, sim, replace),
  tar_target(
    sample_step,
    ssd_run_sample_step(tasks, scenario, "results/sample"),
    format = "file",
    error = "null"
  )
)
fit_targets <- tar_map(
  values = ssd_scenario_fit_shards(scenario),
  names = c(dataset, sim, nrow, rescale),
  tar_target(
    fit_step,
    {
      sample_done
      ssd_run_fit_step(tasks, scenario, "results/sample", "results/fit")
    },
    format = "file",
    error = "null"
  )
)

list(
  sample_targets,
  tar_combine(sample_done, sample_targets),
  fit_targets
)
