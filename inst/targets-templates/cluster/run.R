# Run the ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4).
#
# The package ships this under
# `system.file("targets-templates", "cluster", package = "ssdsims")`. Copy the
# directory's files (`controller.R`, `preflight.R`, `_targets.R`, `run.R`) to
# your project root, edit `controller.R` for your cluster (see the README's
# "zero to a running cluster job" guide and its mapping table), then run this
# driver — interactively with `source("run.R")` or from a shell with
# `Rscript run.R`.
#
# It first runs the connectivity+prerequisite PREFLIGHT (`preflight.R`: one
# SLURM job that checks a worker can run ssdsims) so a wiring problem surfaces
# before the scenario shards, then `tar_make()` dispatches the scenario shards
# across SLURM jobs; each writes one shard
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
# reachable SLURM queue (`sbatch` on PATH). If either is missing, abort with a
# clear message naming the missing prerequisite rather than erroring obscurely.
# To run the same study OFF a cluster (no scheduler), use the `large/` template
# instead: it builds the identical pipeline under a `crew::crew_controller_local()`
# controller.
# ---------------------------------------------------------------------------
missing <- c(
  if (!requireNamespace("crew.cluster", quietly = TRUE)) {
    "`crew.cluster` is not installed (install.packages(\"crew.cluster\"))"
  },
  if (!nzchar(Sys.which("sbatch"))) {
    "no SLURM queue is reachable (`sbatch` not on PATH)"
  }
)
if (length(missing)) {
  stop(
    "Cannot run the cluster pipeline: ",
    paste(missing, collapse = " and "),
    ". Run this from a SLURM login node with `crew.cluster` installed. To run ",
    "the same study off a cluster, use the `large/` template ",
    "(system.file(\"targets-templates\", \"large\", package = \"ssdsims\")).",
    call. = FALSE
  )
}

# Preflight: submit one probe job through the controller and verify the worker
# prerequisites BEFORE any scenario shard. Aborts here on a wiring/prerequisite
# failure (see preflight.R), so the expensive shards never run against a broken
# cluster.
source("preflight.R")

# Build the pipeline. `_targets.R` sources the same controller and calls the
# factory; the scenario shards dispatch across SLURM jobs.
tar_make()

# The `summary` target is `format = "file"`, so its value is the path to the
# combined Parquet (the union of the hc shards).
summary_path <- tar_read(summary)
cat("Summary written to:", summary_path, "\n")

# Peek at the combined estimates (duckplyr is an ssdsims dependency).
print(duckplyr::read_parquet_duckdb(summary_path))
