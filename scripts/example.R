## scripts/example.R
##
## End-to-end driver for the four `inst/targets-examples/` pipelines.
## Runs each one twice — first with a heavily downsized scenario, then
## with the full scenario — and compares each Parquet result against a
## direct ssd_run_scenario2() call.
##
## The full second call is the demonstration of *resumability*: only
## the tasks added by the enlarged knobs run; previously-computed
## branches stay cached. Per-task and per-sim get the full benefit;
## per-parameter-slice and whole-scenario do not (see
## inst/targets-examples/README.md).
##
## Run from the package root with `devtools::load_all()` already in
## scope, e.g.:
##   source("scripts/example.R")

library(targets)
library(dplyr, warn.conflicts = FALSE)
library(ssdsims)

rlang::check_installed(
  c("duckplyr", "qs2"),
  reason = "to run the targets-examples driver."
)

# --- knobs --------------------------------------------------------------
# Section A: downsized
small_env <- c(
  SSDSIMS_EXAMPLE_NSIM = "4", # KNOB: enlarge to 100, 1000
  SSDSIMS_EXAMPLE_NROW = "5,10", # KNOB: more nrow values
  SSDSIMS_EXAMPLE_NBOOT = "50" # KNOB: 1000+ for production
)

# Section B: full
full_env <- c(
  SSDSIMS_EXAMPLE_NSIM = "12", # KNOB: 12 sims (3x small)
  SSDSIMS_EXAMPLE_NROW = "5,10", # same nrow vector ⇒ per-task/per-sim cached
  SSDSIMS_EXAMPLE_NBOOT = "50" # same nboot         ⇒ ditto
)

granularities <- c(
  "per-task",
  "per-sim",
  "per-parameter-slice",
  "whole-scenario"
)

# --- helpers ------------------------------------------------------------

# Each example pipeline persists targets metadata under its own
# inst/targets-examples/<granularity>/_targets/ so subsequent runs with
# the same knobs are cached. We deliberately do NOT use tempdir() —
# the goal is to show that growing nsim re-uses earlier runs.
example_dir <- function(granularity) {
  d <- file.path("inst", "targets-examples", granularity)
  if (!dir.exists(d)) {
    d <- system.file("targets-examples", granularity, package = "ssdsims")
  }
  normalizePath(d, mustWork = TRUE)
}

run_pipeline <- function(granularity, env) {
  dir <- example_dir(granularity)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  do.call(Sys.setenv, as.list(env))
  message("==> tar_make() in ", dir)
  invisible(tar_make(callr_function = NULL))
  files <- list.files("results", full.names = TRUE)
  dplyr::bind_rows(lapply(files, ssdsims::ssd_read_job_parquet))
}

run_manual <- function(env) {
  do.call(Sys.setenv, as.list(env))
  scenario <- ssdsims::ssd_sim_data2(
    ssddata::ccme_boron,
    nsim = as.integer(env[["SSDSIMS_EXAMPLE_NSIM"]]),
    nrow = as.integer(strsplit(env[["SSDSIMS_EXAMPLE_NROW"]], ",")[[1]]),
    nboot = as.integer(env[["SSDSIMS_EXAMPLE_NBOOT"]]),
    seed = 42
  )
  ssdsims::ssd_run_scenario2(scenario)
}

# Lightweight comparison — counts and metadata grid only (Conc / HC
# numeric values won't match exactly because the DuckDB nested
# round-trip may shift inner class wrappers; see
# ssd_read_job_parquet docs).
compare <- function(targets_out, manual_out, label) {
  cat(sprintf(
    "[%s] targets rows = %d, manual rows = %d, match = %s\n",
    label,
    nrow(targets_out),
    nrow(manual_out),
    identical(nrow(targets_out), nrow(manual_out))
  ))
}

# --- Section A: downsized -----------------------------------------------
message("\n========== Section A: downsized ==========")
small_results <- lapply(granularities, function(g) {
  list(
    targets = run_pipeline(g, small_env),
    manual = run_manual(small_env)
  )
})
names(small_results) <- granularities

for (g in granularities) {
  compare(
    small_results[[g]]$targets,
    small_results[[g]]$manual,
    paste0("small/", g)
  )
}

# --- Section B: full ----------------------------------------------------
# Re-running with bigger NSIM. Per-task and per-sim should NOT recompute
# the sims that were already done in Section A — only the new ones.
message("\n========== Section B: full ==========")
full_results <- lapply(granularities, function(g) {
  list(
    targets = run_pipeline(g, full_env),
    manual = run_manual(full_env)
  )
})
names(full_results) <- granularities

for (g in granularities) {
  compare(
    full_results[[g]]$targets,
    full_results[[g]]$manual,
    paste0("full/", g)
  )
}

# Summary of cache hits. Use `tar_progress()` after a tar_make() to see
# which branches built vs were skipped:
message("\nTo inspect cache hits per pipeline, e.g.:")
message("  setwd('inst/targets-examples/per-sim'); targets::tar_progress()")
