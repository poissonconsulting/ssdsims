# Run the ssdsims large targets pipeline.
#
# The package ships this under
# `system.file("targets-templates", "large", package = "ssdsims")`. Copy the
# directory's files (`scenario.R`, `_targets.R`, `run.R`, `run-serial.R`) to your
# project root, edit `scenario.R`, then run this driver — interactively with
# `source("run.R")` or from a shell with `Rscript run.R`. (`run-serial.R` runs
# the same scenario single core, without targets, and compares the results.)
#
# It builds one target per shard (writing
# `results/layout=<hash>/<step>/<axis=value>/.../part.parquet`) and the combined
# `results/layout=<hash>/summary.parquet`, then reports the summary path and a peek at the
# hazard-concentration estimates. This scenario sets `samples = TRUE`, so the
# `summary` target also writes a full `summary-samples.parquet` retaining the
# per-row bootstrap draws (the compact `summary.parquet` projects them out).

library(targets)

# Allow running from the project root (e.g. Positron / VS Code
# Cmd+Shift+Enter), not just from this axis directory.
if (dir.exists("inst/targets-templates/large")) {
  withr::local_dir(
    "inst/targets-templates/large",
    .local_envir = parent.frame(2)
  )
}

# Build the pipeline. `_targets.R` defines one named target per shard and sets a
# mirai-backed `crew` controller, so independent shards run on parallel local
# workers (tune `workers`, or swap a `crew.cluster` controller for SLURM/PBS).
# Needs the `crew` package installed.
tar_make()

# The `summary` target is `format = "file"`. With `samples = TRUE` it tracks two
# files: the compact `summary.parquet` (first) and the full
# `summary-samples.parquet` (second). Peek at the compact estimate table.
summary_paths <- tar_read(summary)
cat("Summaries written to:", summary_paths, sep = "\n")

# Peek at the combined estimates (duckplyr is an ssdsims dependency).
print(duckplyr::read_parquet_duckdb(summary_paths[[1L]]))
