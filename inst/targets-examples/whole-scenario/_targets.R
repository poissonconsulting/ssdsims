# Whole-scenario targets pipeline for ssdsims.
#
# Baseline / control case: one target = one Parquet file for the
# entire scenario. No branching. Any change to any knob re-runs the
# whole pipeline.
#
# See ../README.md for the resumability matrix.

library(targets)

nsim  <- as.integer(Sys.getenv("SSDSIMS_EXAMPLE_NSIM",  "4"))
nrow  <- as.integer(strsplit(
  Sys.getenv("SSDSIMS_EXAMPLE_NROW", "5,10"), ","
)[[1]])
nboot <- as.integer(Sys.getenv("SSDSIMS_EXAMPLE_NBOOT", "50"))

tar_option_set(packages = c("ssdsims", "ssddata", "dplyr", "arrow", "qs2"))

list(
  tar_target(
    scenario,
    ssdsims::ssd_sim_data2(
      ssddata::ccme_boron,
      nsim   = nsim,
      nrow   = nrow,
      nboot  = nboot,
      seed   = 42
    )
  ),

  tar_target(
    job_parquet,
    {
      dir.create("results", showWarnings = FALSE)
      path <- file.path("results", "whole.parquet")
      result <- ssdsims::ssd_run_scenario2(scenario)
      ssdsims::ssd_write_job_parquet(result, path)
      path
    },
    format = "file"
  )
)
