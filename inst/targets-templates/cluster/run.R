# Run the ssdsims cluster targets pipeline (TARGETS-DESIGN.md section 4).
#
# The package ships this under
# `system.file("targets-templates", "cluster", package = "ssdsims")`. Copy the
# directory's files (`scenario.R`, `functions.R`, `_targets.R`, `run.R`,
# `run-serial.R`) to your project root, edit the controller block in `_targets.R`
# for your cluster (see the README's "zero to a running cluster job" guide and
# its mapping table), then run this driver — interactively with `source("run.R")`
# or from a shell with `Rscript run.R`. (`run-serial.R` runs the same scenario
# single core, without targets, and compares the results.)
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
# reachable SLURM queue (`sbatch` on PATH). If either is missing, abort with a
# clear message naming the missing prerequisite rather than erroring obscurely —
# `_targets.R` would otherwise fail when it constructs the SLURM controller. To
# run the same study OFF a cluster (no scheduler), use the `large/` template
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

# Build the pipeline. `_targets.R` sets the SLURM controller, defines the probe
# target, and gates the scenario shards on it, so the probe builds first and a
# probe failure stops the shards.
tar_make()

# The `summary` target is `format = "file"`, so its value is the path to the
# combined Parquet (the union of the hc shards).
summary_path <- tar_read(summary)
cat("Summary written to:", summary_path, "\n")

# Peek at the worker witness the probe returned (R version + node id).
print(tar_read(probe))

# Peek at the combined estimates (duckplyr is an ssdsims dependency).
print(duckplyr::read_parquet_duckdb(summary_path))
