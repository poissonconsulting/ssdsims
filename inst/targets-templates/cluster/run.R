# Run the ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4).
#
# The package ships this under
# `system.file("targets-templates", "cluster", package = "ssdsims")`. Copy the
# directory's files (`scenario.R`, `_targets.R`, `run.R`, `run-serial.R`) to your
# project root, edit the controller block in `_targets.R` for your cluster (see
# the README's "zero to a running cluster job" guide and its mapping table),
# then run this driver — interactively with `source("run.R")` or from a shell
# with `Rscript run.R`. (`run-serial.R` runs the same scenario single core,
# without targets, and compares the results.)
#
# `tar_make()` builds the connectivity+prerequisite probe first (one SLURM job),
# then dispatches the scenario shards across SLURM jobs; each writes one shard
# `results/layout=<hash>/<step>/<axis=value>/.../part.parquet`, then the combined
# `results/layout=<hash>/summary.parquet`. It reports the summary path and a peek
# at the hazard-concentration estimates.

library(targets)

# Allow running from the project root (e.g. Positron / VS Code
# Cmd+Shift+Enter), not just from this cluster directory.
if (dir.exists("inst/targets-templates/cluster")) {
  withr::local_dir(
    "inst/targets-templates/cluster",
    .local_envir = parent.frame(2)
  )
}

# ---------------------------------------------------------------------------
# Guard: the cluster path is opt-in. It needs `crew.cluster` installed AND a
# reachable SLURM queue (`sbatch` on PATH). If either is missing, fall back to
# the off-cluster SMOKE PATH — a `crew::crew_controller_local()` controller (set
# via the `SSDSIMS_CLUSTER_LOCAL` env var that `_targets.R` reads) — so the
# pipeline SHAPE can be validated without a scheduler (mirroring the crew labs'
# local fallback, section 4). If even `crew` is unavailable, skip with a clear
# message rather than erroring obscurely.
# ---------------------------------------------------------------------------
have_crew_cluster <- requireNamespace("crew.cluster", quietly = TRUE)
have_slurm <- nzchar(Sys.which("sbatch"))
have_crew <- requireNamespace("crew", quietly = TRUE)

if (have_crew_cluster && have_slurm) {
  message("Cluster path: crew.cluster + SLURM (`sbatch`) detected.")
  Sys.unsetenv("SSDSIMS_CLUSTER_LOCAL")
} else {
  missing <- c(
    if (!have_crew_cluster) "`crew.cluster` is not installed",
    if (!have_slurm) "no SLURM queue is reachable (`sbatch` not on PATH)"
  )
  if (!have_crew) {
    stop(
      "Cannot run the cluster pipeline: ",
      paste(missing, collapse = " and "),
      ". The off-cluster smoke path needs `crew` (install.packages(\"crew\")). ",
      "On a cluster, install `crew.cluster` and run from a node where `sbatch` ",
      "is on PATH.",
      call. = FALSE
    )
  }
  message(
    "Cluster prerequisites not met (",
    paste(missing, collapse = "; "),
    "). Running the OFF-CLUSTER SMOKE PATH with `crew::crew_controller_local()` ",
    "to validate the pipeline shape. Install `crew.cluster` and run from a ",
    "SLURM login node for the real cluster run."
  )
  Sys.setenv(SSDSIMS_CLUSTER_LOCAL = "1")
}

# Build the pipeline. `_targets.R` sets the controller (SLURM or, on the smoke
# path, local), defines the probe target, and gates the scenario shards on it,
# so the probe builds first and a probe failure stops the shards.
tar_make()

# The `summary` target is `format = "file"`, so its value is the path to the
# combined Parquet (the union of the hc shards).
summary_path <- tar_read(summary)
cat("Summary written to:", summary_path, "\n")

# Peek at the worker witness the probe returned (R version + node id).
print(tar_read(probe))

# Peek at the combined estimates (duckplyr is an ssdsims dependency).
print(duckplyr::read_parquet_duckdb(summary_path))
