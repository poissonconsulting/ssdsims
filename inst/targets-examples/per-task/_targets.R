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

# --- knobs: edit these to change scenario size ---------------------------
# Demonstrates the "downsized first call". To go larger, bump these and
# re-run `targets::tar_make()`; per-task and per-sim pipelines retain
# their cache for already-computed branches (see ../README.md).
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

  # `stable_config` extracts only the bits each task needs from the
  # scenario. Its output is content-stable across `nsim` bumps so that
  # downstream branches stay cached when only `nsim` grows.
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
    format = "file"
  )
)
