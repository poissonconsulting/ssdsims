## scripts/example-targets.R
##
## Side-by-side comparison of the four `inst/targets-examples/`
## pipelines against direct `ssd_run_scenario2()` calls.
##
## The pipelines hold their own knobs (nsim/nrow/nboot) at the top of
## each `_targets.R`. To enlarge a pipeline, edit those knobs and
## re-run `targets::tar_make()`; the per-task and per-sim pipelines
## retain their cache for already-computed branches (see
## inst/targets-examples/README.md for the resumability matrix).
##
## This driver does NOT inject knobs into the pipelines — those live in
## the pipeline files. `small_env` / `full_env` below drive the
## *manual* comparison runs of `ssd_run_scenario2()` only.

library(targets)
library(dplyr, warn.conflicts = FALSE)
library(ssdsims)

rlang::check_installed(
  c("duckplyr", "qs2"),
  reason = "to run the targets-examples driver."
)

# --- manual-run knobs ---------------------------------------------------
# small_env should match the values currently hardcoded in each
# _targets.R so the pipeline output and manual_small are directly
# comparable.
small_env <- list(
  nsim = 4L,
  nrow = c(5L, 10L),
  nboot = 50L
)

# full_env is for the larger manual run — illustrates how the API
# scales but does not feed the pipelines.
full_env <- list(
  nsim = 12L,
  nrow = c(5L, 10L),
  nboot = 50L
)

granularities <- c(
  "per-task",
  "per-sim",
  "per-parameter-slice",
  "whole-scenario"
)

# --- helpers ------------------------------------------------------------

example_dir <- function(granularity) {
  d <- file.path("inst", "targets-examples", granularity)
  if (!dir.exists(d)) {
    d <- system.file("targets-examples", granularity, package = "ssdsims")
  }
  normalizePath(d, mustWork = TRUE)
}

run_pipeline <- function(granularity) {
  dir <- example_dir(granularity)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  message("==> tar_make() in ", dir)
  invisible(tar_make(callr_function = NULL))
  files <- list.files("results", full.names = TRUE)
  dplyr::bind_rows(lapply(files, ssdsims::ssd_read_job_parquet))
}

run_manual <- function(env) {
  scenario <- ssdsims::ssd_sim_data2(
    ssddata::ccme_boron,
    nsim = env$nsim,
    nrow = env$nrow,
    nboot = env$nboot,
    seed = 42
  )
  ssdsims::ssd_run_scenario2(scenario)
}

compare <- function(targets_out, manual_out, label) {
  cat(sprintf(
    "[%s] targets rows = %d, manual rows = %d, match = %s\n",
    label,
    nrow(targets_out),
    nrow(manual_out),
    identical(nrow(targets_out), nrow(manual_out))
  ))
}

# --- Pipeline runs (use whatever knobs are in each _targets.R) ---------
message("\n========== Running each pipeline once ==========")
pipeline_results <- lapply(granularities, run_pipeline)
names(pipeline_results) <- granularities

# --- Manual side-by-side -----------------------------------------------
message("\n========== Manual: small ==========")
manual_small <- run_manual(small_env)
for (g in granularities) {
  compare(
    pipeline_results[[g]],
    manual_small,
    paste0("pipeline/", g, " vs manual_small")
  )
}

message("\n========== Manual: full ==========")
manual_full <- run_manual(full_env)
cat(sprintf("manual_full rows = %d\n", nrow(manual_full)))

message(
  "\nTo grow a pipeline incrementally, edit the `nsim`/`nrow`/`nboot` ",
  "block at the top of its `_targets.R` and re-run ",
  "`targets::tar_make()`."
)
