# Test fixture: a tiny static-branching pipeline over a saved numeric dataset
# (data.rds), so the worker and the in-process baseline run the identical
# scenario. Used by the byte-identity oracle test in test-task-shards.R.
library(targets)
library(tarchetypes)
library(ssdsims)

scenario <- ssd_define_scenario(
  ssd_data(d = readRDS("data.rds")),
  nsim = 2L,
  nrow = c(5L, 10L),
  seed = 42L,
  rescale = c(FALSE, TRUE),
  dists = c("lnorm", "gamma")
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
hc_targets <- tar_map(
  values = ssd_scenario_hc_shards(scenario),
  names = c(dataset, sim),
  tar_target(
    hc_step,
    {
      fit_done
      ssd_run_hc_step(tasks, scenario, "results/fit", "results/hc")
    },
    format = "file",
    error = "null"
  )
)

list(
  sample_targets,
  tar_combine(sample_done, sample_targets),
  fit_targets,
  tar_combine(fit_done, fit_targets),
  hc_targets,
  tar_combine(hc_done, hc_targets),
  tar_target(
    summary,
    {
      hc_done
      ssd_summarize(
        "results/sample",
        "results/fit",
        "results/hc",
        "results/summary.parquet"
      )
    },
    format = "file"
  )
)
