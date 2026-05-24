# Per-sim targets pipeline for ssdsims.
#
# One target branch per sim id = one Parquet file per sim. Growing
# `nsim` adds new branches without invalidating existing ones (same
# story as per-task, just coarser units).
#
# See ../README.md for the resumability matrix.

library(targets)
library(tarchetypes)

nsim <- as.integer(Sys.getenv("SSDSIMS_EXAMPLE_NSIM", "4"))
nrow <- as.integer(strsplit(
  Sys.getenv("SSDSIMS_EXAMPLE_NROW", "5,10"),
  ","
)[[1]])
nboot <- as.integer(Sys.getenv("SSDSIMS_EXAMPLE_NBOOT", "50"))

tar_option_set(packages = c("ssdsims", "ssddata", "dplyr", "duckplyr", "qs2"))

list(
  tar_target(
    scenario,
    ssdsims::ssd_sim_data2(
      ssddata::ccme_boron,
      nsim = nsim,
      nrow = nrow,
      nboot = nboot,
      seed = 42
    )
  ),

  tar_target(
    stable_config,
    structure(
      list(
        fit = list(dists = scenario$fit$dists),
        hc = list(
          proportion = scenario$hc$proportion,
          ci = scenario$hc$ci
        ),
        extras = scenario$extras
      ),
      class = "ssdsims_scenario"
    )
  ),

  tar_target(tasks, ssdsims::ssd_scenario_tasks(scenario)),

  tar_group_by(task_groups, tasks, sim),

  tar_target(
    job_parquet,
    {
      dir.create("results", showWarnings = FALSE)
      path <- file.path(
        "results",
        sprintf("sim_%04d.parquet", task_groups$sim[1])
      )
      result <- ssdsims::ssd_run_job(task_groups, stable_config)
      ssdsims::ssd_write_job_parquet(result, path)
      path
    },
    pattern = map(task_groups),
    format = "file"
  )
)
