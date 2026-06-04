# Run the ssdsims example targets pipeline.
#
# The package ships this under
# `system.file("targets-templates", "local", package = "ssdsims")`. Copy the
# directory's files (`scenario.R`, `_targets.R`, `run.R`, `run-serial.R`) to your
# project root, edit `scenario.R`, then run this driver — interactively with
# `source("run.R")` or from a shell with `Rscript run.R`. (`run-serial.R` runs
# the same scenario single core, without targets, and compares the results.)
#
# It builds one target per shard (writing
# `results/<step>/<axis=value>/.../part.parquet`) and the combined
# `results/summary.parquet`, then reports the summary path and a peek at the
# hazard-concentration estimates.

library(targets)

# Allow running from the project root (e.g. Positron / VS Code
# Cmd+Shift+Enter), not just from this axis directory.
if (dir.exists("inst/targets-templates/example")) {
  withr::local_dir("inst/targets-templates/example", .local_envir = parent.frame(2))
}

# Build the pipeline. `_targets.R` defines one named target per shard, so
# independent shards can run in parallel; to scale out, configure a controller
# (e.g. `crew` / `crew.cluster`) via `tar_option_set()` in `_targets.R` — the
# shard set and `tar_make()` call here are unchanged.
tar_make()

# The `summary` target is `format = "file"`, so its value is the path to the
# combined Parquet (the union of the hc shards).
summary_path <- tar_read(summary)
cat("Summary written to:", summary_path, "\n")

# Peek at the combined estimates (duckplyr is an ssdsims dependency).
print(duckplyr::read_parquet_duckdb(summary_path))
