# Per-task targets pipeline for ssdsims.
#
# One target branch per task = one Parquet file per task = finest
# resolution targets can track. Adding new sims (or any new tasks)
# creates new branches; existing branches stay cached because their
# task-row content is content-hashed by targets and reproducible from
# the master seed.
#
# See ../README.md for the resumability matrix.

library(targets)
library(tarchetypes)

# --- knobs ---------------------------------------------------------------
# Read by *every* example so the same env vars drive all four
# granularities side-by-side. See `scripts/example.R`.
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

  # `stable_config` extracts only the bits each task needs from the
  # scenario. Its output is content-stable across `nsim` bumps so that
  # downstream branches stay cached when only `nsim` grows.
  tar_target(
    stable_config,
    structure(
      list(
        fit = list(dists = scenario$fit$dists),
        hc  = list(
          proportion = scenario$hc$proportion,
          ci         = scenario$hc$ci
        ),
        extras = scenario$extras
      ),
      class = "ssdsims_scenario"
    )
  ),

  tar_target(
    tasks,
    {
      t <- ssdsims::ssd_scenario_tasks(scenario)
      t$task_id <- seq_len(nrow(t))
      t
    }
  ),

  # tarchetypes::tar_group_by() returns a grouped target. With
  # `pattern = map(task_groups)` downstream, each branch sees exactly
  # one group's rows.
  tar_group_by(task_groups, tasks, task_id),

  tar_target(
    job_parquet,
    {
      dir.create("results", showWarnings = FALSE)
      path <- file.path(
        "results",
        sprintf("task_%04d.parquet", task_groups$task_id[1])
      )
      result <- ssdsims::ssd_run_job(task_groups, stable_config)
      ssdsims::ssd_write_job_parquet(result, path)
      path
    },
    pattern = map(task_groups),
    format  = "file"
  )
)
