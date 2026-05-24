# Whole-scenario targets pipeline for ssdsims.
#
# Baseline / control case: one target = one Parquet file for the
# entire scenario. No branching. Any change to any knob re-runs the
# whole pipeline.
#
# See ../README.md for the resumability matrix.

library(targets)

# --- knobs: edit these to change scenario size ---------------------------
nsim <- 4L # KNOB: enlarge to 100, 1000, …
nrow <- c(5L, 10L) # KNOB: more nrow values
nboot <- 50L # KNOB: 1000+ for production
# -------------------------------------------------------------------------

tar_option_set(
  packages = c("ssdsims", "ssddata", "dplyr", "duckplyr", "qs2"),
  controller = crew::crew_controller_local(
    workers = parallelly::availableCores(),
    seconds_idle = 30
  )
)

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
